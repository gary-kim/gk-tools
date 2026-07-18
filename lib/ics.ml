open! Core

type t =
  { uid : string
  ; cleaned : string
  }

let uid t = t.uid
let cleaned t = t.cleaned

let unfold raw =
  String.split_lines raw
  |> List.fold ~init:[] ~f:(fun acc line ->
    if String.is_empty line
    then acc
    else if String.is_prefix line ~prefix:" " || String.is_prefix line ~prefix:"\t"
    then (
      let cont = String.drop_prefix line 1 in
      match acc with
      | last :: rest -> [%string "%{last}%{cont}"] :: rest
      | [] -> cont :: acc)
    else line :: acc)
  |> List.rev
;;

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

let unquoted_colon_index str =
  String.fold_until
    str
    ~init:(0, false)
    ~f:(fun (idx, in_quotes) c ->
      match c with
      | '"' -> Continue (idx + 1, not in_quotes)
      | ':' when not in_quotes -> Stop (Some idx)
      | _ -> Continue (idx + 1, in_quotes))
    ~finish:(const None)
;;

let uid_value raw =
  let value =
    if String.Caseless.is_prefix raw ~prefix:"UID:"
       || String.Caseless.is_prefix raw ~prefix:"UID;"
    then
      unquoted_colon_index raw
      |> Option.map ~f:(fun idx -> String.strip (String.drop_prefix raw (idx + 1)))
    else None
  in
  Option.filter value ~f:(Fn.non String.is_empty)
;;

let allowed_vcalendar_properties =
  lazy (String.Set.of_list [ "VERSION"; "PRODID"; "CALSCALE" ])
;;

let required_vcalendar_properties = lazy (String.Set.of_list [ "VERSION"; "PRODID" ])

module Parse_state = struct
  type t =
    { open_components : string list (* stack; head is innermost open component *)
    ; acc : string list (* reversed; kept lines for [cleaned] *)
    ; uids : string list (* reversed *)
    ; vcalendar_properties : String.Set.t
    }
end

let step (state : Parse_state.t) raw : Parse_state.t Or_error.t =
  match classify raw with
  | Begin name ->
    Ok
      { state with
        open_components = name :: state.open_components
      ; acc = raw :: state.acc
      }
  | End name ->
    (match state.open_components with
     | [] -> Or_error.error_s [%message "unexpected END outside component" ~line:raw]
     | open_name :: rest ->
       if not (String.Caseless.equal open_name name)
       then
         Or_error.error_s
           [%message
             "mismatched BEGIN/END component"
               ~opened:(open_name : string)
               ~closed:(name : string)]
       else Ok { state with open_components = rest; acc = raw :: state.acc })
  | Property name ->
    let depth = List.length state.open_components in
    let keep =
      depth > 0
      || Set.mem (force allowed_vcalendar_properties) name
      || String.is_prefix name ~prefix:"X-"
    in
    let acc = if keep then raw :: state.acc else state.acc in
    let uids =
      (* RFC 5545: UID belongs on primary components, not nested ones like VALARM. *)
      if String.equal name "UID" && depth = 1
      then
        Option.value_map (uid_value raw) ~default:state.uids ~f:(fun uid ->
          uid :: state.uids)
      else state.uids
    in
    let vcalendar_properties =
      if depth = 0
      then Set.add state.vcalendar_properties name
      else state.vcalendar_properties
    in
    Ok { state with acc; uids; vcalendar_properties }
  | Malformed _ ->
    let depth = List.length state.open_components in
    let acc = if depth > 0 then raw :: state.acc else state.acc in
    Ok { state with acc }
;;

let parse data =
  let open Or_error.Let_syntax in
  let lines = unfold data in
  let%bind first, inner, last =
    let too_short =
      Error.create_s [%message "iCalendar data too short" (data : string)]
    in
    match lines with
    | first :: rest ->
      let%bind last = List.last rest |> Or_error.of_option ~error:too_short in
      let%bind inner = List.drop_last rest |> Or_error.of_option ~error:too_short in
      if not (String.Caseless.equal (String.strip first) "BEGIN:VCALENDAR")
      then Or_error.error_s [%message "expected BEGIN:VCALENDAR" ~got:first]
      else if not (String.Caseless.equal (String.strip last) "END:VCALENDAR")
      then Or_error.error_s [%message "expected END:VCALENDAR" ~got:last]
      else Ok (first, inner, last)
    | _ -> Error too_short
  in
  let init : Parse_state.t =
    { open_components = []
    ; acc = [ first ]
    ; uids = []
    ; vcalendar_properties = String.Set.empty
    }
  in
  let%bind final = List.fold_result inner ~init ~f:step in
  let%bind () =
    match final.open_components with
    | [] -> Ok ()
    | unclosed ->
      Or_error.error_s [%message "unterminated component" (unclosed : string list)]
  in
  let%bind () =
    let missing =
      Set.diff (force required_vcalendar_properties) final.vcalendar_properties
    in
    if Set.is_empty missing
    then Ok ()
    else
      Or_error.error_s
        [%message "VCALENDAR missing required properties" (missing : String.Set.t)]
  in
  let%bind uid =
    match final.uids with
    | [] -> Or_error.error_s [%message "no UID found in iCalendar data"]
    | [ uid ] -> Ok uid
    | uids ->
      Or_error.error_s
        [%message
          "multiple UIDs found; multi-event files not yet supported"
            ~count:(List.length uids : int)]
  in
  let cleaned = String.concat ~sep:"\r\n" (List.rev (last :: final.acc)) in
  Ok { uid; cleaned = [%string "%{cleaned}\r\n"] }
;;
