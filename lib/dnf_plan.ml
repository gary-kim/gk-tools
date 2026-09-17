open! Core

module Reason = struct
  type t =
    | User
    | Group
    | External_user
    | Dependency
    | Weak_dependency
    | Clean
    | Unknown
  [@@deriving sexp, to_string ~capitalize:"Title Case"]

  let of_tag tag =
    match tag with
    | 0 -> Ok Unknown
    | 1 -> Ok Dependency
    | 2 -> Ok User
    | 3 -> Ok Clean
    | 4 -> Ok Weak_dependency
    | 5 -> Ok Group
    | 6 -> Ok External_user
    | _ -> Or_error.error_s [%message "unknown reason tag" (tag : int)]
  ;;

  let is_explicit reason =
    match reason with
    | User | Group | External_user -> true
    | Dependency | Weak_dependency | Clean | Unknown -> false
  ;;
end

module Modifier = struct
  type t =
    | Group
    | Only_hostname_regex of Host_pattern.t
  [@@deriving sexp]
end

module Entry = struct
  type t =
    { name : string
    ; modifiers : Modifier.t list
    }

  let t_of_sexp sexp =
    let name, modifiers =
      match (sexp : Sexp.t) with
      | Atom name -> name, []
      | List (Atom name :: modifiers) ->
        name, List.map modifiers ~f:[%of_sexp: Modifier.t]
      | List _ ->
        Sexplib0.Sexp_conv.of_sexp_error "entry must be NAME or (NAME MODIFIER ...)" sexp
    in
    if String.is_prefix name ~prefix:"-"
    then Sexplib0.Sexp_conv.of_sexp_error "entry name may not start with [-]" sexp
    else { name; modifiers }
  ;;

  let sexp_of_t { name; modifiers } =
    match modifiers with
    | [] -> Sexp.Atom name
    | _ :: _ -> Sexp.List (Atom name :: List.map modifiers ~f:[%sexp_of: Modifier.t])
  ;;

  let is_group t =
    List.exists t.modifiers ~f:(fun modifier ->
      match modifier with
      | Group -> true
      | Only_hostname_regex _ -> false)
  ;;

  let applies t ~hostname =
    List.for_all t.modifiers ~f:(fun modifier ->
      match modifier with
      | Group -> true
      | Only_hostname_regex pattern -> Host_pattern.matches pattern ~hostname)
  ;;
end

module Snapshot = struct
  type t =
    { packages : Reason.t Map.M(String).t
    ; groups : Set.M(String).t Map.M(String).t
    ; environments : Set.M(String).t Map.M(String).t
    ; installed_groups : Set.M(String).t
    ; installed_environments : Set.M(String).t
    }
  [@@deriving sexp]
end

let installed_of_alist pairs =
  Map.of_alist_reduce (module String) pairs ~f:(fun a b ->
    Bool.select (Reason.is_explicit a) a b)
;;

module Resolved_group = struct
  type t =
    { name : string
    ; covered_ids : string list
    ; members : Set.M(String).t
    ; installed : bool
    }
  [@@deriving sexp_of]
end

module Warning = struct
  type t =
    | Group_not_installed of { name : string }
    | Installed_group_not_declared of { id : string }
  [@@deriving sexp_of]
end

type t =
  { install : string list
  ; mark_user : (string * Reason.t) list
  ; mark_dependency : (string * Reason.t) list
  ; warnings : Warning.t list
  }
[@@deriving sexp_of]

let compute ~packages ~groups ~installed ~installed_group_ids =
  let wanted = Set.of_list (module String) packages in
  let protected =
    Set.union_list
      (module String)
      (List.map groups ~f:(fun (group : Resolved_group.t) -> group.members))
  in
  let install =
    Set.filter wanted ~f:(fun name -> not (Map.mem installed name)) |> Set.to_list
  in
  let mark_user =
    Map.to_alist installed
    |> List.filter ~f:(fun (name, reason) ->
      Set.mem wanted name && not (Reason.is_explicit reason))
  in
  let mark_dependency =
    Map.to_alist installed
    |> List.filter ~f:(fun (name, reason) ->
      Reason.is_explicit reason
      && (not (Set.mem wanted name))
      && not (Set.mem protected name))
  in
  let warnings =
    let not_installed =
      List.filter_map groups ~f:(fun (group : Resolved_group.t) ->
        Option.some_if
          (not group.installed)
          (Warning.Group_not_installed { name = group.name }))
    in
    let covered =
      List.concat_map groups ~f:(fun (group : Resolved_group.t) -> group.covered_ids)
      |> Set.of_list (module String)
    in
    let undeclared =
      Set.diff installed_group_ids covered
      |> Set.to_list
      |> List.map ~f:(fun id -> Warning.Installed_group_not_declared { id })
    in
    not_installed @ undeclared
  in
  { install; mark_user; mark_dependency; warnings }
;;

let resolve_group (snapshot : Snapshot.t) ~name =
  match Map.find snapshot.groups name with
  | Some members ->
    Ok
      { Resolved_group.name
      ; covered_ids = [ name ]
      ; members
      ; installed = Set.mem snapshot.installed_groups name
      }
  | None ->
    let open Or_error.Let_syntax in
    let%bind group_ids =
      Map.find snapshot.environments name
      |> Or_error.of_option
           ~error:
             (Error.create_s [%message "unknown group or environment" (name : string)])
    in
    let missing = Set.filter group_ids ~f:(fun id -> not (Map.mem snapshot.groups id)) in
    let%map () =
      Result.ok_if_true
        (Set.is_empty missing)
        ~error:
          (Error.create_s
             [%message
               "environment references groups missing from current comps"
                 (name : string)
                 (missing : Set.M(String).t)])
    in
    { Resolved_group.name
    ; covered_ids = name :: Set.to_list group_ids
    ; members =
        Set.union_list
          (module String)
          (Set.to_list group_ids |> List.filter_map ~f:(Map.find snapshot.groups))
    ; installed = Set.mem snapshot.installed_environments name
    }
;;

let plan (snapshot : Snapshot.t) ~entries ~hostname =
  let open Or_error.Let_syntax in
  let group_entries, package_entries =
    List.filter entries ~f:(Entry.applies ~hostname)
    |> List.partition_tf ~f:Entry.is_group
  in
  let%map groups =
    List.map group_entries ~f:(fun (entry : Entry.t) ->
      resolve_group snapshot ~name:entry.name)
    |> Or_error.combine_errors
  in
  compute
    ~packages:(List.map package_entries ~f:(fun (entry : Entry.t) -> entry.name))
    ~groups
    ~installed:snapshot.packages
    ~installed_group_ids:
      (Set.union snapshot.installed_groups snapshot.installed_environments)
;;

let dump_entries (snapshot : Snapshot.t) =
  let resolve ids =
    Set.to_list ids
    |> List.partition_map ~f:(fun id ->
      match resolve_group snapshot ~name:id with
      | Ok group -> First group
      | Error _ -> Second id)
  in
  let environments, omitted_environments = resolve snapshot.installed_environments in
  let groups, omitted_groups = resolve snapshot.installed_groups in
  let covered_by_environments =
    List.concat_map environments ~f:(fun (group : Resolved_group.t) -> group.covered_ids)
    |> Set.of_list (module String)
  in
  let members =
    List.map (environments @ groups) ~f:(fun (group : Resolved_group.t) -> group.members)
    |> Set.union_list (module String)
  in
  let group_entry (group : Resolved_group.t) =
    { Entry.name = group.name; modifiers = [ Modifier.Group ] }
  in
  let entries =
    List.map environments ~f:group_entry
    @ (List.filter groups ~f:(fun (group : Resolved_group.t) ->
         not (Set.mem covered_by_environments group.name))
       |> List.map ~f:group_entry)
    @ (Map.to_alist snapshot.packages
       |> List.filter_map ~f:(fun (name, reason) ->
         Option.some_if
           (Reason.is_explicit reason && not (Set.mem members name))
           { Entry.name; modifiers = [] }))
  in
  entries, omitted_environments @ omitted_groups
;;

let has_actions t =
  not
    (List.is_empty t.install
     && List.is_empty t.mark_user
     && List.is_empty t.mark_dependency)
;;

let to_lines t =
  let section title entries ~f =
    match entries with
    | [] -> []
    | _ :: _ -> title :: List.map entries ~f:(fun entry -> "  " ^ f entry)
  in
  let with_reason (name, reason) = [%string "%{name} (%{reason#Reason})"] in
  let warning_line (warning : Warning.t) =
    match warning with
    | Group_not_installed { name } ->
      [%string "warning: declared group is not installed: %{name}"]
    | Installed_group_not_declared { id } ->
      [%string "warning: installed group is not in config: %{id}"]
  in
  let actions =
    section "install:" t.install ~f:Fn.id
    @ section "mark user:" t.mark_user ~f:with_reason
    @ section "mark dependency:" t.mark_dependency ~f:with_reason
  in
  List.map t.warnings ~f:warning_line
  @ if List.is_empty actions then [ "nothing to do" ] else actions
;;
