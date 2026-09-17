open! Core

type t =
  { pattern : string
  ; negated : bool
  ; re : Re.re
  }

let of_string_or_error pattern =
  let negated, body =
    match String.chop_prefix pattern ~prefix:"!" with
    | Some body -> true, body
    | None -> false, pattern
  in
  if String.is_empty body
  then Or_error.error_s [%message "empty pattern" (pattern : string)]
  else (
    match Re.Pcre.re_result body with
    | Error (`Not_supported | `Parse_error) ->
      Or_error.error_s [%message "invalid pattern" (pattern : string)]
    | Ok re -> Ok { pattern; negated; re = Re.compile (Re.whole_string re) })
;;

let of_string pattern = of_string_or_error pattern |> Or_error.ok_exn
let to_string t = t.pattern
let matches t ~hostname = Bool.( <> ) (Re.execp t.re hostname) t.negated

include (
  Sexpable.Of_stringable (struct
    type nonrec t = t

    let of_string = of_string
    let to_string = to_string
  end) :
    Sexpable.S with type t := t)

let hostname_param =
  let open Command.Let_syntax in
  let%map_open hostname =
    flag
      "hostname"
      (optional string)
      ~doc:"HOST override the hostname used for host pattern matching"
  in
  Option.value_or_thunk hostname ~default:Core_unix.gethostname
;;
