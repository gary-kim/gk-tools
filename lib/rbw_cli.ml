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

let rbw args = Process.run ~prog:"rbw" ~args ()
let sync () = rbw [ "sync" ] |> Deferred.Or_error.ignore_m

let is_unlocked () =
  Process.run ~prog:"rbw" ~args:[ "unlocked" ] () |> Deferred.map ~f:Result.is_ok
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
  String.chop_suffix output ~suffix:"\n" |> Option.value ~default:output
;;

let edit_with_content ~folder ~name ~contents =
  let%bind () = validated_args [ "folder", folder; "name", name ] in
  Process.run
    ~prog:"rbw"
    ~args:[ "edit"; "--folder"; folder; "--"; name ]
    ~stdin:contents
    ()
  |> Deferred.Or_error.ignore_m
  |> Deferred.map ~f:(Or_error.tag ~tag:"rbw edit failed")
;;
