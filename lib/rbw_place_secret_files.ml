open! Core
open! Async
open Deferred.Or_error.Let_syntax

let folder = "SECRET_FILES"
let prefix = folder ^ "/"
let rbw args = Process.run ~prog:"rbw" ~args ()

let sync () =
  let%bind (_ : string) = rbw [ "sync" ] in
  return ()
;;

let list_records () =
  let%bind output = rbw [ "search"; "--folder"; folder; "" ] in
  String.split_lines output
  |> List.filter_map ~f:(fun line -> String.chop_prefix (String.strip line) ~prefix)
  |> return
;;

let get_filepath record =
  let%bind output =
    Process.run
      ~prog:"rbw"
      ~args:[ "get"; "--folder"; folder; "--field"; "filepath"; record ]
      ~accept_nonzero_exit:[ 1 ]
      ()
  in
  match String.strip output with
  | "" -> return None
  | path -> return (Some path)
;;

let get_contents record = rbw [ "get"; "--folder"; folder; record ]

let resolve_target filepath =
  let prepend_home rest =
    match Sys.getenv "HOME" with
    | Some h -> Ok (if String.is_empty rest then h else h ^/ rest)
    | None -> Or_error.error_string "HOME environment variable not set"
  in
  if Filename.is_absolute filepath
  then Ok filepath
  else if String.equal filepath "~"
  then prepend_home ""
  else (
    match String.chop_prefix filepath ~prefix:"~/" with
    | Some rest -> prepend_home rest
    | None -> prepend_home filepath)
;;

let diff_config () =
  let output =
    if Core_unix.isatty Core_unix.stdout
    then Patdiff_kernel.Output.Ansi
    else Patdiff_kernel.Output.Ascii
  in
  Patdiff.Configuration.override Patdiff.Configuration.default ~output
;;

let apply_record ~target ~contents =
  let%bind () =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      let%bind.Deferred () =
        Unix.mkdir ~p:() ~perm:0o700 (Filename.dirname target)
      in
      Writer.save target ~contents ~perm:0o600)
  in
  [%log.info "wrote" (target : string)];
  return ()
;;

let dry_run_record ~target ~contents ~record =
  match%bind.Deferred Sys.file_exists target with
  | `No | `Unknown ->
    [%log.info "file does not exist locally, printing contents" (target : string)];
    print_string contents;
    return ()
  | `Yes ->
    let%bind local_content =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.file_contents target)
    in
    (match
       Patdiff.Compare_core.diff_strings
         (diff_config ())
         ~prev:{ name = target; text = local_content }
         ~next:{ name = [%string "bitwarden:%{record}"]; text = contents }
     with
     | `Same ->
       [%log.info "file matches" (target : string)];
       return ()
     | `Different diff ->
       print_endline diff;
       return ())
;;

let handle_record ~apply record =
  let%bind filepath_opt = get_filepath record in
  match filepath_opt with
  | None ->
    [%log.info "record missing filepath field" (record : string)];
    return ()
  | Some filepath ->
    let%bind target = resolve_target filepath |> Deferred.return in
    let%bind contents = get_contents record in
    if apply
    then apply_record ~target ~contents
    else dry_run_record ~target ~contents ~record
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Place secret files from Bitwarden (via rbw) at their configured locations"
    (let%map_open.Command apply =
       flag "apply" no_arg ~doc:" actually place files (default is dry-run)"
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind () = sync () in
       let%bind records = list_records () in
       Deferred.Or_error.List.iter records ~how:`Sequential ~f:(handle_record ~apply))
;;
