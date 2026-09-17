open! Core
module Package_name : Identifiable.S = String
module Group_id : Identifiable.S = String

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
  module Target = struct
    type t =
      | Package of Package_name.t
      | Group of Group_id.t
  end

  type t =
    { target : Target.t
    ; host_patterns : Host_pattern.t list
    }

  let t_of_sexp sexp =
    let name, modifiers =
      match (sexp : Sexp.t) with
      | Atom name -> name, []
      | List (Atom name :: modifiers) ->
        name, List.map modifiers ~f:[%of_sexp: Modifier.t]
      | List _ -> of_sexp_error "entry must be NAME or (NAME MODIFIER ...)" sexp
    in
    if String.is_prefix name ~prefix:"-"
    then of_sexp_error "entry name may not start with [-]" sexp
    else (
      let is_group =
        List.exists modifiers ~f:(fun modifier ->
          match modifier with
          | Group -> true
          | Only_hostname_regex _ -> false)
      in
      let host_patterns =
        List.filter_map modifiers ~f:(fun modifier ->
          match modifier with
          | Group -> None
          | Only_hostname_regex pattern -> Some pattern)
      in
      let target : Target.t =
        if is_group
        then Group (Group_id.of_string name)
        else Package (Package_name.of_string name)
      in
      { target; host_patterns })
  ;;

  let sexp_of_t { target; host_patterns } =
    let name, group_modifier =
      match target with
      | Package name -> Package_name.to_string name, []
      | Group id -> Group_id.to_string id, [ Modifier.Group ]
    in
    let host_modifiers =
      List.map host_patterns ~f:(fun pattern -> Modifier.Only_hostname_regex pattern)
    in
    match group_modifier @ host_modifiers with
    | [] -> Sexp.Atom name
    | modifiers -> Sexp.List (Atom name :: List.map modifiers ~f:[%sexp_of: Modifier.t])
  ;;

  let applies t ~hostname =
    List.for_all t.host_patterns ~f:(fun pattern ->
      Host_pattern.matches pattern ~hostname)
  ;;
end

module Snapshot = struct
  type t =
    { packages : Reason.t Map.M(Package_name).t
    ; groups : Set.M(Package_name).t Map.M(Group_id).t
    ; environments : Set.M(Group_id).t Map.M(Group_id).t
    ; installed_groups : Set.M(Group_id).t
    ; installed_environments : Set.M(Group_id).t
    }
  [@@deriving sexp]
end

let installed_of_alist pairs =
  Map.of_alist_reduce (module Package_name) pairs ~f:(fun a b ->
    Bool.select (Reason.is_explicit a) a b)
;;

module Resolved_group = struct
  type t =
    { name : Group_id.t
    ; covered_ids : Group_id.t list
    ; members : Set.M(Package_name).t
    ; installed : bool
    }
  [@@deriving sexp_of]
end

module Warning = struct
  type t =
    | Group_not_installed of { name : Group_id.t }
    | Installed_group_not_declared of { id : Group_id.t }
  [@@deriving sexp_of]
end

module Step = struct
  module Action = struct
    type t =
      | Install
      | Mark_user
      | Mark_dependency
  end

  type t =
    { action : Action.t
    ; packages : Package_name.t Nonempty_list.t
    }
end

type t =
  { install : Package_name.t list
  ; mark_user : (Package_name.t * Reason.t) list
  ; mark_dependency : (Package_name.t * Reason.t) list
  ; warnings : Warning.t list
  }
[@@deriving sexp_of]

let compute ~packages ~groups ~installed ~installed_group_ids =
  let wanted = Set.of_list (module Package_name) packages in
  let protected =
    Set.union_list
      (module Package_name)
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
      |> Set.of_list (module Group_id)
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
    let%bind.Or_error group_ids =
      Map.find snapshot.environments name
      |> Or_error.of_option
           ~error:
             (Error.create_s
                [%message "unknown group or environment" (name : Group_id.t)])
    in
    let missing = Set.filter group_ids ~f:(fun id -> not (Map.mem snapshot.groups id)) in
    let%map.Or_error () =
      Result.ok_if_true
        (Set.is_empty missing)
        ~error:
          (Error.of_lazy_sexp
             [%lazy_message
               "environment references groups missing from current comps"
                 (name : Group_id.t)
                 (missing : Set.M(Group_id).t)])
    in
    { Resolved_group.name
    ; covered_ids = name :: Set.to_list group_ids
    ; members =
        Set.union_list
          (module Package_name)
          (Set.to_list group_ids |> List.filter_map ~f:(Map.find snapshot.groups))
    ; installed = Set.mem snapshot.installed_environments name
    }
;;

let plan (snapshot : Snapshot.t) ~entries ~hostname =
  let packages, group_ids =
    List.filter entries ~f:(Entry.applies ~hostname)
    |> List.partition_map ~f:(fun (entry : Entry.t) ->
      match entry.target with
      | Package name -> First name
      | Group id -> Second id)
  in
  let%map.Or_error groups =
    List.map group_ids ~f:(fun name -> resolve_group snapshot ~name)
    |> Or_error.combine_errors
  in
  compute
    ~packages
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
    |> Set.of_list (module Group_id)
  in
  let members =
    List.map (environments @ groups) ~f:(fun (group : Resolved_group.t) -> group.members)
    |> Set.union_list (module Package_name)
  in
  let group_entry (group : Resolved_group.t) =
    { Entry.target = Group group.name; host_patterns = [] }
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
           { Entry.target = Package name; host_patterns = [] }))
  in
  entries, omitted_environments @ omitted_groups
;;

let steps { install; mark_user; mark_dependency; warnings = _ } =
  [ Step.Action.Install, install
  ; Mark_user, List.map mark_user ~f:fst
  ; Mark_dependency, List.map mark_dependency ~f:fst
  ]
  |> List.filter_map ~f:(fun (action, packages) ->
    Nonempty_list.of_list packages
    |> Option.map ~f:(fun packages -> { Step.action; packages }))
;;

let section_lines ~title entries =
  match entries with
  | [] -> []
  | _ :: _ -> title :: List.map entries ~f:(fun entry -> [%string "  %{entry}"])
;;

let to_lines { install; mark_user; mark_dependency; warnings } =
  let section title entries ~f = section_lines ~title (List.map entries ~f) in
  let with_reason (name, reason) = [%string "%{name#Package_name} (%{reason#Reason})"] in
  let warning_line (warning : Warning.t) =
    match warning with
    | Group_not_installed { name } ->
      [%string "warning: declared group is not installed: %{name#Group_id}"]
    | Installed_group_not_declared { id } ->
      [%string "warning: installed group is not in config: %{id#Group_id}"]
  in
  let actions =
    section "install:" install ~f:Package_name.to_string
    @ section "mark user:" mark_user ~f:with_reason
    @ section "mark dependency:" mark_dependency ~f:with_reason
  in
  List.map warnings ~f:warning_line
  @ if List.is_empty actions then [ "nothing to do" ] else actions
;;
