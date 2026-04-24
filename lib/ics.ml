open! Core

type t =
  { uid : string
  ; cleaned : string
  }

let uid t = t.uid
let cleaned t = t.cleaned

module Parser = struct
  open Angstrom

  let not_eol c = Char.(c <> '\r' && c <> '\n')
  let line_segment = take_while not_eol
  let fold_marker = (string "\r\n" <|> string "\n") *> (char ' ' <|> char '\t')
  let line_end = string "\r\n" *> return () <|> (char '\n' *> return ()) <|> end_of_input

  let content_line =
    let* () =
      peek_char
      >>= function
      | None -> fail "eof"
      | Some _ -> return ()
    in
    let* first = line_segment in
    let* rest = many (fold_marker *> line_segment) in
    let* () = line_end in
    return (String.concat (first :: rest))
  ;;

  let parser = many content_line >>| List.filter ~f:(fun s -> not (String.is_empty s))
end

module Line_kind = struct
  type t =
    | Begin of string
    | End of string
    | Property of string
    | Malformed of string
end

let property_name upper_line =
  match String.lsplit2 upper_line ~on:':' with
  | None -> None
  | Some (before_colon, _) ->
    Some
      (match String.lsplit2 before_colon ~on:';' with
       | Some (name, _) -> name
       | None -> before_colon)
;;

let classify raw : Line_kind.t =
  if String.Caseless.is_prefix raw ~prefix:"BEGIN:"
  then Begin (String.strip (String.drop_prefix raw (String.length "BEGIN:")))
  else if String.Caseless.is_prefix raw ~prefix:"END:"
  then End (String.strip (String.drop_prefix raw (String.length "END:")))
  else (
    match property_name (String.uppercase raw) with
    | Some name -> Property name
    | None -> Malformed raw)
;;

let uid_value raw =
  let value =
    if String.Caseless.is_prefix raw ~prefix:"UID:"
    then Some (String.strip (String.drop_prefix raw (String.length "UID:")))
    else if String.Caseless.is_prefix raw ~prefix:"UID;"
    then (
      let upper = String.uppercase raw in
      String.substr_index upper ~pattern:":" ~pos:(String.length "UID;")
      |> Option.map ~f:(fun idx -> String.strip (String.drop_prefix raw (idx + 1))))
    else None
  in
  Option.filter value ~f:(Fn.non String.is_empty)
;;

let allowed_vcalendar_properties = String.Set.of_list [ "VERSION"; "PRODID"; "CALSCALE" ]

(* RFC 5545 §3.4: every iCalendar object MUST include these. *)
let required_vcalendar_properties = String.Set.of_list [ "VERSION"; "PRODID" ]

module Parse_state = struct
  type t =
    { depth : int
    ; acc : string list (* reversed; kept lines for [cleaned] *)
    ; uids : string list (* reversed *)
    ; vcalendar_properties : String.Set.t
    }
end

let step (state : Parse_state.t) raw : Parse_state.t Or_error.t =
  match classify raw with
  | Begin _ -> Ok { state with depth = state.depth + 1; acc = raw :: state.acc }
  | End _ ->
    if state.depth <= 0
    then Or_error.error_s [%message "unexpected END outside component" ~line:raw]
    else Ok { state with depth = state.depth - 1; acc = raw :: state.acc }
  | Property name ->
    let keep = state.depth > 0 || Set.mem allowed_vcalendar_properties name in
    let acc = if keep then raw :: state.acc else state.acc in
    let uids =
      if String.equal name "UID"
      then (
        match uid_value raw with
        | Some uid -> uid :: state.uids
        | None -> state.uids)
      else state.uids
    in
    let vcalendar_properties =
      if state.depth = 0
      then Set.add state.vcalendar_properties name
      else state.vcalendar_properties
    in
    Ok { state with acc; uids; vcalendar_properties }
  | Malformed _ ->
    let acc = if state.depth > 0 then raw :: state.acc else state.acc in
    Ok { state with acc }
;;

let parse data =
  let open Or_error.Let_syntax in
  let%bind lines =
    match Angstrom.parse_string ~consume:All Parser.parser data with
    | Ok lines -> Ok lines
    | Error msg -> Or_error.error_s [%message "failed to parse ICS data" (msg : string)]
  in
  let%bind first, inner, last =
    match lines with
    | first :: (_ :: _ as rest) ->
      let last = List.last_exn rest in
      let inner = List.drop_last_exn rest in
      if not (String.Caseless.equal (String.strip first) "BEGIN:VCALENDAR")
      then Or_error.error_s [%message "expected BEGIN:VCALENDAR" ~got:first]
      else if not (String.Caseless.equal (String.strip last) "END:VCALENDAR")
      then Or_error.error_s [%message "expected END:VCALENDAR" ~got:last]
      else Ok (first, inner, last)
    | _ -> Or_error.error_string "iCalendar data too short"
  in
  let init : Parse_state.t =
    { depth = 0
    ; acc = [ first ]
    ; uids = []
    ; vcalendar_properties = String.Set.empty
    }
  in
  let%bind final = List.fold_result inner ~init ~f:step in
  let%bind () =
    if final.depth = 0 then Ok () else Or_error.error_string "unterminated component"
  in
  let%bind () =
    let missing =
      Set.diff required_vcalendar_properties final.vcalendar_properties
    in
    if Set.is_empty missing
    then Ok ()
    else
      Or_error.error_s
        [%message "VCALENDAR missing required properties" (missing : String.Set.t)]
  in
  let%bind uid =
    match List.dedup_and_sort final.uids ~compare:String.compare with
    | [] -> Or_error.error_string "no UID found in iCalendar data"
    | [ uid ] -> Ok uid
    | uids ->
      Or_error.error_s
        [%message
          "multiple UIDs found; multi-event files not yet supported"
            ~count:(List.length uids : int)]
  in
  Ok
    { uid
    ; cleaned =
        String.concat ~sep:"\r\n" (List.rev (last :: final.acc)) ^ "\r\n"
    }
;;
