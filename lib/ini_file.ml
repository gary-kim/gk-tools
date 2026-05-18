open! Core
open! Async

module Section = struct
  type t =
    { name : string
    ; entries : string Map.M(String).t
    }
end

type t = Section.t list

module Fold_state = struct
  type t =
    { closed : Section.t list (* reversed *)
    ; current : string option
    ; entries : string Map.M(String).t
    }
end

let close_section (state : Fold_state.t) =
  match state.current with
  | Some name -> { Section.name; entries = state.entries } :: state.closed
  | None ->
    if Map.is_empty state.entries
    then state.closed
    else { Section.name = ""; entries = state.entries } :: state.closed
;;

let step (state : Fold_state.t) line =
  let stripped = String.strip line in
  if String.is_empty stripped
  then state
  else if String.is_prefix stripped ~prefix:"#" || String.is_prefix stripped ~prefix:";"
  then state
  else if String.is_prefix stripped ~prefix:"["
  then (
    match
      String.chop_prefix stripped ~prefix:"["
      |> Option.bind ~f:(fun rest -> String.lsplit2 rest ~on:']')
    with
    | Some (name, _) ->
      { closed = close_section state
      ; current = Some (String.strip name)
      ; entries = Map.empty (module String)
      }
    | None ->
      [%log.info "ignoring malformed INI section header" (line : string)];
      state)
  else (
    match String.lsplit2 stripped ~on:'=' with
    | Some (k, v) ->
      { state with
        entries = Map.set state.entries ~key:(String.strip k) ~data:(String.strip v)
      }
    | None ->
      [%log.info "ignoring malformed INI line (missing '=')" (line : string)];
      state)
;;

let parse_string contents =
  let init : Fold_state.t =
    { closed = []; current = None; entries = Map.empty (module String) }
  in
  String.split_lines contents |> List.fold ~init ~f:step |> close_section |> List.rev
;;

let load path =
  let%bind.Deferred.Or_error contents =
    Deferred.Or_error.try_with ~extract_exn:true (fun () -> Reader.file_contents path)
  in
  Deferred.Or_error.return (parse_string contents)
;;

let get t ~section ~key =
  List.find_map t ~f:(fun (s : Section.t) ->
    if String.equal s.name section then Map.find s.entries key else None)
;;

let find_key t ~key =
  List.find_map t ~f:(fun (s : Section.t) ->
    Map.find s.entries key |> Option.map ~f:(fun value -> s.name, value))
;;
