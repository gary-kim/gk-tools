open! Core
open! Async
open Deferred.Or_error.Let_syntax

let folder = "SECRET_FILES"

module Resolved = struct
  type t =
    { name : string
    ; local_target : string
    ; remote_content : string
    }
  [@@deriving sexp_of]
end

let resolve_target ~home_dir filepath =
  if Filename.is_absolute filepath
  then filepath
  else (
    let rest =
      if String.equal filepath "~"
      then ""
      else Option.value (String.chop_prefix filepath ~prefix:"~/") ~default:filepath
    in
    if String.is_empty rest then home_dir else home_dir ^/ rest)
;;

let resolve ~home_dir (record : Rbw_cli.Record.t) : Resolved.t Or_error.t =
  match Rbw_cli.Record.field record "filepath" with
  | None ->
    Or_error.error_s
      [%message "record missing filepath field" ~name:(record.name : string)]
  | Some filepath ->
    Ok
      { Resolved.name = record.name
      ; local_target = resolve_target ~home_dir filepath
      ; remote_content = Option.value record.notes ~default:""
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
  let%bind () =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      let%bind.Deferred () =
        Unix.mkdir ~p:() ~perm:0o700 (Filename.dirname resolved.local_target)
      in
      Writer.save resolved.local_target ~contents:resolved.remote_content ~perm:0o600)
  in
  [%log.info "wrote" ~target:(resolved.local_target : string)];
  return ()
;;

let check_record_matches (resolved : Resolved.t) =
  match%bind.Deferred Sys.file_exists resolved.local_target with
  | `No | `Unknown ->
    [%log.info
      "file does not exist locally, printing contents"
        ~target:(resolved.local_target : string)];
    print_string resolved.remote_content;
    return false
  | `Yes ->
    let%bind local_content =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.file_contents resolved.local_target)
    in
    (match
       Patdiff.Compare_core.diff_strings
         (diff_config ())
         ~prev:{ name = resolved.local_target; text = local_content }
         ~next:
           { name = [%string "bitwarden:%{resolved.name}"]
           ; text = resolved.remote_content
           }
     with
     | `Same ->
       [%log.info "file matches" ~target:(resolved.local_target : string)];
       return true
     | `Different diff ->
       print_endline diff;
       return false)
;;

let find_record_by_id ~id =
  let%bind candidates = Rbw_cli.search ~folder ~term:"" in
  let exact = List.filter candidates ~f:(fun name -> String.Caseless.equal name id) in
  match exact with
  | [ name ] -> return name
  | [] ->
    Deferred.Or_error.error_s
      [%message "no exact match for id" ~id:(id : string) (candidates : string list)]
  | _ ->
    Deferred.Or_error.error_s
      [%message
        "ambiguous id (multiple exact matches)" ~id:(id : string) (exact : string list)]
;;

let upload ~yes ~home_dir ~id =
  let%bind () = Rbw_cli.sync () in
  let%bind matched_name = find_record_by_id ~id in
  let%bind record = Rbw_cli.get ~folder ~name:matched_name in
  let%bind resolved = resolve ~home_dir record |> Deferred.return in
  let%bind local_content =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.file_contents resolved.local_target)
  in
  match
    Patdiff.Compare_core.diff_strings
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
    let%bind confirmed =
      if yes
      then return true
      else Deferred.ok (Async_interactive.ask_yn ~default:false "Upload?")
    in
    if confirmed
    then (
      let%bind () =
        Rbw_cli.edit_with_content ~folder ~name:resolved.name ~contents:local_content
      in
      [%log.info "uploaded" ~name:(resolved.name : string)];
      return ())
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

let resolve_record ~home_dir name =
  let%bind record = Rbw_cli.get ~folder ~name in
  resolve ~home_dir record |> Deferred.return
;;

let apply_all ~home_dir =
  let%bind () = Rbw_cli.sync () in
  let%bind records = Rbw_cli.search ~folder ~term:"" in
  Deferred.Or_error.List.iter records ~how:`Sequential ~f:(fun name ->
    let%bind resolved = resolve_record ~home_dir name in
    apply_record resolved)
;;

let check_all ~home_dir =
  let%bind () = Rbw_cli.sync () in
  let%bind records = Rbw_cli.search ~folder ~term:"" in
  let%bind matches =
    Deferred.Or_error.List.map records ~how:`Sequential ~f:(fun name ->
      let%bind resolved = resolve_record ~home_dir name in
      check_record_matches resolved)
  in
  if List.for_all matches ~f:Fn.id
  then return ()
  else Deferred.Or_error.error_s [%message "files differ from Bitwarden"]
;;

let check_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Show the diff between local files and their Bitwarden contents"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       check_all ~home_dir)
;;

let apply_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Place secret files from Bitwarden at their configured locations"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       apply_all ~home_dir)
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

let upload_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Upload a local file's bytes back into its rbw record"
    (let%map_open.Command home_dir_or_error = home_dir_param
     and yes = flag "yes" no_arg ~doc:" skip the confirmation prompt"
     and id = anon ("ID" %: id_arg_type)
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind home_dir = Deferred.return home_dir_or_error in
       upload ~yes ~home_dir ~id)
;;

let command =
  Command.group
    ~summary:"Manage SECRET_FILES records via rbw"
    [ "check", check_command; "apply", apply_command; "upload", upload_command ]
;;

module For_testing = struct
  module Resolved = Resolved

  let resolve_target = resolve_target
  let resolve = resolve
end
