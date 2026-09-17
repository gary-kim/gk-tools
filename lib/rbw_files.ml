open! Core
open! Async
open Deferred.Or_error.Let_syntax

let folder = "SECRET_FILES"
let default_perm = 0o600

module Resolved = struct
  type t =
    { name : string
    ; local_target : string
    ; remote_content : string
    ; mode : int option
    }
  [@@deriving sexp_of]
end

let mode_re = lazy (Re.compile Re.(whole_string (repn (rg '0' '7') 1 (Some 4))))

let parse_mode str =
  let open Or_error.Let_syntax in
  let invalid_mode_error =
    Error.of_lazy_sexp
      [%lazy_message "invalid mode (must be 1-4 octal digits)" ~mode:(str : string)]
  in
  let%bind () =
    Result.ok_if_true (Re.execp (force mode_re) str) ~error:invalid_mode_error
  in
  [%string "0o%{str}"]
  |> Int.of_string_opt
  |> Or_error.of_option ~error:invalid_mode_error
;;

let format_perm perm = Printf.sprintf "0o%o" perm

let resolve_target ~home_dir filepath =
  if Filename.is_absolute filepath
  then filepath
  else (
    let rest = Option.value (Home_path.chop_tilde filepath) ~default:filepath in
    Home_path.under_home ~home:home_dir ~rest)
;;

let record_content (record : Rbw_cli.Record.t) = Option.value record.notes ~default:""

let record_filepath (record : Rbw_cli.Record.t) =
  Rbw_cli.Record.field record "filepath"
  |> Or_error.of_option_lazy_sexp
       ~error:[%lazy_message "record missing filepath field" ~name:(record.name : string)]
;;

module Host_check = struct
  type t =
    | Applies
    | Mismatch of { pattern : string }
  [@@deriving sexp_of]
end

let host_pattern (record : Rbw_cli.Record.t) =
  List.find record.fields ~f:(fun (f : Rbw_cli.Field.t) ->
    Option.exists f.name ~f:(String.equal "hosts_regex"))
  |> Option.value_map ~default:(Ok None) ~f:(fun field ->
    let open Or_error.Let_syntax in
    let%bind pattern =
      field.value
      |> Or_error.of_option ~error:(Error.create_s [%message "field has no value"])
    in
    Host_pattern.of_string_or_error pattern |> Or_error.map ~f:Option.some)
  |> Or_error.tag_s ~tag:[%message "invalid hosts_regex" ~name:(record.name : string)]
;;

let host_check ~hostname record =
  host_pattern record
  |> Or_error.map ~f:(fun pattern ->
    match pattern with
    | Some pattern when not (Host_pattern.matches pattern ~hostname) ->
      Host_check.Mismatch { pattern = Host_pattern.to_string pattern }
    | None | Some _ -> Host_check.Applies)
;;

let resolve ?(strict = true) ~home_dir (record : Rbw_cli.Record.t) : Resolved.t Or_error.t
  =
  let open Or_error.Let_syntax in
  let%bind filepath = record_filepath record in
  let%map mode =
    match Rbw_cli.Record.field record "mode" with
    | None -> Ok None
    | Some str ->
      (match parse_mode str with
       | Ok mode -> Ok (Some mode)
       | Error err ->
         if strict
         then Error err
         else (
           [%log.warn
             "ignoring invalid mode field for upload"
               ~name:(record.name : string)
               (err : Error.t)];
           Ok None))
  in
  { Resolved.name = record.name
  ; local_target = resolve_target ~home_dir filepath
  ; remote_content = record_content record
  ; mode
  }
;;

let diff_config () =
  let output =
    if Core_unix.isatty Core_unix.stdout
    then Patdiff_kernel.Output.Ansi
    else Patdiff_kernel.Output.Ascii
  in
  Patdiff.Configuration.override Patdiff.Configuration.default ~output
;;

let apply_record (resolved : Resolved.t) =
  let perm = Option.value resolved.mode ~default:default_perm in
  let chmod_to_target () =
    let%map () =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Unix.chmod resolved.local_target ~perm)
    in
    [%log.info
      "chmod" ~target:(resolved.local_target : string) ~perm:(format_perm perm : string)];
    ()
  in
  let write () =
    let%bind () =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        let%bind.Deferred () =
          Unix.mkdir ~p:() ~perm:0o700 (Filename.dirname resolved.local_target)
        in
        Writer.save
          resolved.local_target
          ~contents:resolved.remote_content
          ~perm
          ~fsync:true)
    in
    [%log.info
      "wrote" ~target:(resolved.local_target : string) ~perm:(format_perm perm : string)];
    Option.value_map resolved.mode ~default:(return ()) ~f:(fun _ -> chmod_to_target ())
  in
  match%bind.Deferred Sys.file_exists resolved.local_target with
  | `No -> write ()
  | `Unknown ->
    Deferred.Or_error.error_s
      [%message
        "cannot determine if file exists" ~target:(resolved.local_target : string)]
  | `Yes ->
    let%bind local_content =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.file_contents resolved.local_target)
    in
    if not (String.equal local_content resolved.remote_content)
    then write ()
    else (
      match resolved.mode with
      | None ->
        [%log.info "file already matches" ~target:(resolved.local_target : string)];
        return ()
      | Some _ ->
        let%bind stats =
          Deferred.Or_error.try_with ~extract_exn:true (fun () ->
            Unix.stat resolved.local_target)
        in
        if stats.perm land 0o7777 = perm land 0o7777
        then (
          [%log.info "file already matches" ~target:(resolved.local_target : string)];
          return ())
        else chmod_to_target ())
;;

let check_record_matches (resolved : Resolved.t) =
  let diff_from_local ~local_content =
    Patdiff.Compare_core.diff_strings
      ~print_global_header:true
      (diff_config ())
      ~prev:{ name = resolved.local_target; text = local_content }
      ~next:
        { name = [%string "bitwarden:%{resolved.name}"]; text = resolved.remote_content }
  in
  match%bind.Deferred Sys.file_exists resolved.local_target with
  | `Unknown ->
    Deferred.Or_error.error_s
      [%message
        "cannot determine if file exists" ~target:(resolved.local_target : string)]
  | `No ->
    [%log.info "file does not exist locally" ~target:(resolved.local_target : string)];
    (match diff_from_local ~local_content:"" with
     | `Different diff -> print_endline diff
     | `Same -> ());
    return false
  | `Yes ->
    let%bind local_content =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.file_contents resolved.local_target)
    in
    (match diff_from_local ~local_content with
     | `Different diff ->
       print_endline diff;
       return false
     | `Same ->
       (match resolved.mode with
        | None ->
          [%log.info "file matches" ~target:(resolved.local_target : string)];
          return true
        | Some target_perm ->
          let%map stats =
            Deferred.Or_error.try_with ~extract_exn:true (fun () ->
              Unix.stat resolved.local_target)
          in
          let actual = stats.perm land 0o7777 in
          let expected = target_perm land 0o7777 in
          if actual = expected
          then (
            [%log.info "file matches" ~target:(resolved.local_target : string)];
            true)
          else (
            [%log.info
              "mode mismatch"
                ~target:(resolved.local_target : string)
                ~expected:(format_perm expected : string)
                ~actual:(format_perm actual : string)];
            false)))
;;

let find_record_name_in_list ~records ~id =
  match List.filter records ~f:(String.Caseless.equal id) with
  | [ name ] -> Or_error.return name
  | [] -> Or_error.error_s [%message "no record matching id" (id : string)]
  | matches ->
    Or_error.error_s [%message "ambiguous id" (id : string) (matches : string list)]
;;

let find_record_names_in_list ~records ~ids =
  Nonempty_list.map ids ~f:(fun id -> find_record_name_in_list ~records ~id)
  |> Nonempty_list.to_list
  |> Or_error.combine_errors
;;

let find_record_by_id ~id =
  let%bind candidates = Rbw_cli.search ~folder ~term:"" in
  find_record_name_in_list ~records:candidates ~id |> Deferred.return
;;

let upload ~yes ~home_dir ~hostname ~id =
  let%bind () = Rbw_cli.sync () in
  let%bind matched_name = find_record_by_id ~id in
  let%bind record = Rbw_cli.get ~folder ~name:matched_name in
  let%bind () =
    match%bind host_check ~hostname record |> Deferred.return with
    | Applies -> return ()
    | Mismatch { pattern } ->
      Deferred.Or_error.error_s
        [%message
          "record does not apply to this host"
            ~name:(matched_name : string)
            (pattern : string)
            (hostname : string)]
  in
  let%bind resolved = resolve ~strict:false ~home_dir record |> Deferred.return in
  let%bind local_content =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.file_contents resolved.local_target)
  in
  match
    Patdiff.Compare_core.diff_strings
      ~print_global_header:true
      (diff_config ())
      ~prev:
        { name = [%string "bitwarden:%{resolved.name}"]; text = resolved.remote_content }
      ~next:{ name = resolved.local_target; text = local_content }
  with
  | `Same ->
    [%log.info "no changes to upload" ~name:(resolved.name : string)];
    return ()
  | `Different diff ->
    print_endline diff;
    let%bind confirmed = Confirm.ask ~yes ~prompt:"Upload?" in
    if confirmed
    then (
      let%map () =
        Rbw_cli.edit_with_content ~folder ~name:resolved.name ~contents:local_content
      in
      [%log.info "uploaded" ~name:(resolved.name : string)];
      ())
    else (
      [%log.info "upload skipped" ~name:(resolved.name : string)];
      return ())
;;

let resolve_home_dir cli_value =
  match Option.first_some cli_value (Sys.getenv "HOME") with
  | Some dir -> Or_error.return dir
  | None -> Or_error.error_s [%message "no home directory: pass -home-dir or set $HOME"]
;;

let home_dir_param =
  let open Command.Let_syntax in
  let%map_open home_dir =
    flag
      "home-dir"
      (optional string)
      ~doc:"DIR override $HOME for ~/ and relative-path expansion"
  in
  resolve_home_dir home_dir
;;

let cat ~id =
  let%bind () = Rbw_cli.sync () in
  let%bind name = find_record_by_id ~id in
  let%map record = Rbw_cli.get ~folder ~name in
  print_string (record_content record)
;;

let apply ~home_dir ~hostname ~ids =
  let%bind () = Rbw_cli.sync () in
  let%bind records = Rbw_cli.search ~folder ~term:"" in
  let explicit = Option.is_some ids in
  let%bind to_apply =
    match ids with
    | None -> return records
    | Some ids -> find_record_names_in_list ~records ~ids |> Deferred.return
  in
  Deferred.Or_error.List.iter to_apply ~how:`Sequential ~f:(fun name ->
    let%bind record = Rbw_cli.get ~folder ~name in
    match%bind host_check ~hostname record |> Deferred.return with
    | Applies ->
      let%bind resolved = resolve ~home_dir record |> Deferred.return in
      apply_record resolved
    | Mismatch { pattern } ->
      if explicit
      then
        Deferred.Or_error.error_s
          [%message
            "record does not apply to this host"
              (name : string)
              (pattern : string)
              (hostname : string)]
      else (
        [%log.info
          "skipping (host mismatch)"
            (name : string)
            (hostname : string)
            (pattern : string)];
        return ()))
;;

let check_all ~home_dir ~hostname =
  let%bind () = Rbw_cli.sync () in
  let%bind records = Rbw_cli.search ~folder ~term:"" in
  let check_one name : (string * bool) option Deferred.Or_error.t =
    let%bind record = Rbw_cli.get ~folder ~name in
    match%bind host_check ~hostname record |> Deferred.return with
    | Mismatch { pattern } ->
      [%log.info
        "skipping (host mismatch)" (name : string) (hostname : string) (pattern : string)];
      return None
    | Applies ->
      let%bind resolved = resolve ~home_dir record |> Deferred.return in
      let%map matched = check_record_matches resolved in
      Some (name, matched)
  in
  let%bind.Deferred per_record =
    Deferred.List.map records ~how:`Sequential ~f:check_one
  in
  let mismatched =
    List.filter_map per_record ~f:(function
      | Ok (Some (name, false)) -> Some name
      | _ -> None)
  in
  let errors = List.filter_map per_record ~f:Result.error in
  match mismatched, errors with
  | [], [] -> return ()
  | mismatched, [] ->
    Deferred.Or_error.error_s
      [%message "files differ from Bitwarden" (mismatched : string list)]
  | mismatched, _ :: _ ->
    Deferred.Or_error.error_s
      [%message "errors during check" (mismatched : string list) (errors : Error.t list)]
;;

let verify_records ~home_dir =
  let%bind () = Rbw_cli.sync () in
  let%bind records = Rbw_cli.search ~folder ~term:"" in
  let verify_one name =
    let%bind record = Rbw_cli.get ~folder ~name in
    Or_error.combine_errors_unit
      [ host_pattern record |> Or_error.ignore_m
      ; resolve ~home_dir record |> Or_error.ignore_m
      ]
    |> Deferred.return
  in
  Deferred.List.map records ~how:`Sequential ~f:(fun name ->
    verify_one name
    |> Deferred.Or_error.tag_s ~tag:[%message "invalid record" (name : string)])
  |> Deferred.map ~f:Or_error.combine_errors_unit
;;

let check_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Show the diff between local files and their Bitwarden contents"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and hostname = Host_pattern.hostname_param
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       check_all ~home_dir ~hostname)
;;

let verify_records_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Validate every record's metadata regardless of host scoping"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       verify_records ~home_dir)
;;

let id_arg_type =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      Thread_safe.block_on_async_exn (fun () ->
        match%bind.Deferred Rbw_cli.is_unlocked () with
        | false -> Deferred.return []
        | true ->
          (match%map.Deferred Rbw_cli.search ~folder ~term:part with
           | Error _ -> []
           | Ok candidates ->
             List.filter candidates ~f:(fun name ->
               String.Caseless.is_prefix name ~prefix:part))))
    Fn.id
;;

let apply_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Place secret files from Bitwarden at their configured locations"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and hostname = Host_pattern.hostname_param
     and ids = anon (sequence ("ID" %: id_arg_type))
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let ids = Nonempty_list.of_list ids in
       let%bind home_dir = Deferred.return home_dir_or_error in
       apply ~home_dir ~hostname ~ids)
;;

let cat_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Print secret file contents from Bitwarden"
    (let%map_open.Command id = anon ("ID" %: id_arg_type)
     and () = Log.Global.set_level_via_param () in
     fun () -> cat ~id)
;;

let upload_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Upload a local file's bytes back into its rbw record"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and hostname = Host_pattern.hostname_param
     and yes = flag "yes" no_arg ~doc:" skip the confirmation prompt"
     and id = anon ("ID" %: id_arg_type)
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       upload ~yes ~home_dir ~hostname ~id)
;;

let readme () =
  String.strip
    {|
Records in the SECRET_FILES Bitwarden folder describe secret files: the
record's notes hold the file contents, and custom fields control placement:

filepath      required; ~/ and relative paths resolve under $HOME
mode          optional octal permissions (1-4 digits); 0600 when absent
hosts_regex   optional regex scoping the record to matching hosts; matched
              against the whole hostname (FQDN); a leading ! inverts the
              match; records without the field apply everywhere
|}
;;

let command =
  Command.group
    ~summary:"Manage SECRET_FILES records via rbw"
    ~readme
    [ "check", check_command
    ; "apply", apply_command
    ; "cat", cat_command
    ; "upload", upload_command
    ; "verify-records", verify_records_command
    ]
;;

module For_testing = struct
  module Resolved = Resolved

  let resolve_target = resolve_target
  let resolve = resolve
  let apply_record = apply_record
end
