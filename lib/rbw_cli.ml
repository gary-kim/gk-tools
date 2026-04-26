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

let search ~folder ~term =
  let%bind output = rbw [ "search"; "--folder"; folder; "--"; term ] in
  let prefix = folder ^ "/" in
  String.split_lines output |> List.filter_map ~f:(String.chop_prefix ~prefix) |> return
;;

let get ~folder ~name =
  let%bind output = rbw [ "get"; "--raw"; "--folder"; folder; "--"; name ] in
  Or_error.try_with (fun () -> Jsonaf.of_string output |> Record.t_of_jsonaf)
  |> Or_error.tag ~tag:"failed to parse rbw record JSON"
  |> Deferred.return
;;

let edit_with_content ~folder ~name ~contents =
  Process.run
    ~prog:"rbw"
    ~args:[ "edit"; "--folder"; folder; "--"; name ]
    ~stdin:contents
    ()
  |> Deferred.Or_error.ignore_m
  |> Deferred.map ~f:(Or_error.tag ~tag:"rbw edit failed")
;;
