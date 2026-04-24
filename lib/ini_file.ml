open! Core
open! Async

module Entry = struct
  type t =
    | Section of string
    | Binding of string * string
end

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

let parser =
  let open Angstrom in
  let not_eol c = Char.(c <> '\n' && c <> '\r') in
  let ws =
    skip_while (function
      | ' ' | '\t' -> true
      | _ -> false)
  in
  let eol = end_of_line <|> end_of_input in
  let rest = skip_while not_eol in
  let entry : Entry.t option Angstrom.t =
    ws *> peek_char
    >>= function
    | None -> fail "eof"
    | Some '[' ->
      char '['
      *> take_while1 (fun c -> not_eol c && Char.( <> ) c ']')
      <* char ']'
      <* rest
      <* eol
      >>| fun name -> Some (Entry.Section (String.strip name))
    | Some '#' | Some ';' -> rest *> eol *> return None
    | Some '\n' | Some '\r' -> eol *> return None
    | Some _ ->
      take_while not_eol
      <* eol
      >>| fun line ->
      let stripped = String.strip line in
      if String.is_empty stripped
      then None
      else (
        match String.lsplit2 stripped ~on:'=' with
        | Some (k, v) -> Some (Entry.Binding (String.strip k, String.strip v))
        | None ->
          [%log.info "ignoring malformed INI line (missing '=')" (line : string)];
          None)
  in
  many entry >>| List.filter_opt
;;

let group_entries (input : Entry.t list) =
  let close_section (state : Fold_state.t) =
    match state.current with
    | Some name -> { Section.name; entries = state.entries } :: state.closed
    | None ->
      if Map.is_empty state.entries
      then state.closed
      else { Section.name = ""; entries = state.entries } :: state.closed
  in
  let init : Fold_state.t =
    { closed = []; current = None; entries = Map.empty (module String) }
  in
  let final =
    List.fold input ~init ~f:(fun (state : Fold_state.t) (entry : Entry.t) ->
      match entry with
      | Section name ->
        { closed = close_section state
        ; current = Some name
        ; entries = Map.empty (module String)
        }
      | Binding (key, value) ->
        { state with entries = Map.set state.entries ~key ~data:value })
  in
  List.rev (close_section final)
;;

let load path =
  let%bind.Deferred.Or_error contents =
    Deferred.Or_error.try_with ~extract_exn:true (fun () -> Reader.file_contents path)
  in
  match Angstrom.parse_string ~consume:All parser contents with
  | Error msg -> Deferred.Or_error.error_string msg
  | Ok entries -> Deferred.Or_error.return (group_entries entries)
;;

let get t ~section ~key =
  List.find_map t ~f:(fun (s : Section.t) ->
    if String.equal s.name section then Map.find s.entries key else None)
;;

let find_key t ~key =
  List.find_map t ~f:(fun (s : Section.t) ->
    Map.find s.entries key |> Option.map ~f:(fun value -> s.name, value))
;;
