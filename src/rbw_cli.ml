open! Core
open! Async
open Deferred.Or_error.Let_syntax
open Jsonaf.Export

module Field = struct
  type t =
    { name : string option [@default None]
    ; value : string option [@default None]
    }
  [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
end

module Record = struct
  type t =
    { name : string
    ; fields : Field.t list [@default []]
    ; notes : string option [@default None]
    }
  [@@deriving jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

  let field t name =
    List.find_map t.fields ~f:(fun (f : Field.t) ->
      match f.name with
      | Some n when String.equal n name -> f.value
      | _ -> None)
  ;;
end

let rbw args = Logged_process.run ~prog:"rbw" ~args ()

let sync () =
  let%bind process = Logged_process.create ~prog:"rbw" ~args:[ "sync" ] () in
  let finished = Process.collect_output_and_wait process in
  match%bind.Deferred Clock_ns.with_timeout (Time_ns.Span.of_int_sec 10) finished with
  | `Result { exit_status = Ok (); _ } -> return ()
  | `Result output ->
    [%log.warn
      "rbw sync failed; continuing with cached vault" (output : Process.Output.t)];
    return ()
  | `Timeout ->
    Process.send_signal process Signal.int;
    don't_wait_for (Deferred.ignore_m finished);
    [%log.warn "rbw sync timed out; continuing with cached vault"];
    return ()
;;

let is_unlocked () =
  Logged_process.run ~prog:"rbw" ~args:[ "unlocked" ] () |> Deferred.map ~f:Result.is_ok
;;

let check_no_dash_prefix ~label arg =
  if String.is_prefix arg ~prefix:"-"
  then
    Or_error.error_s
      [%message "rbw argument may not start with [-]" (label : string) (arg : string)]
  else Ok arg
;;

let validated_args args =
  List.map args ~f:(fun (label, arg) -> check_no_dash_prefix ~label arg)
  |> Or_error.combine_errors
  |> Or_error.ignore_m
  |> Deferred.return
;;

let search ~folder ~term =
  let%bind () = validated_args [ "folder", folder; "term", term ] in
  let%map output = rbw [ "search"; "--folder"; folder; "--"; term ] in
  let prefix = [%string "%{folder}/"] in
  String.split_lines output |> List.filter_map ~f:(String.chop_prefix ~prefix)
;;

let get ~folder ~name =
  let%bind () = validated_args [ "folder", folder; "name", name ] in
  let%bind output = rbw [ "get"; "--raw"; "--folder"; folder; "--"; name ] in
  Or_error.try_with (fun () -> Jsonaf.of_string output |> Record.t_of_jsonaf)
  |> Or_error.tag ~tag:"failed to parse rbw record JSON"
  |> Deferred.return
;;

let get_field ~folder ~name ~field =
  let%bind () = validated_args [ "folder", folder; "name", name; "field", field ] in
  let%map output = rbw [ "get"; "--field"; field; "--folder"; folder; "--"; name ] in
  String.chop_suffix_if_exists output ~suffix:"\n"
;;

let edit_with_content ~folder ~name ~contents =
  let%bind () = validated_args [ "folder", folder; "name", name ] in
  Logged_process.run
    ~prog:"rbw"
    ~args:[ "edit"; "--folder"; folder; "--"; name ]
    ~stdin:contents
    ()
  |> Deferred.Or_error.ignore_m
  |> Deferred.map ~f:(Or_error.tag ~tag:"rbw edit failed")
;;
