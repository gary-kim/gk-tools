open! Core
open! Async

let ask ~yes ~prompt =
  if yes
  then Deferred.Or_error.return true
  else (
    match%bind.Deferred
      Deferred.both (Unix.isatty (Fd.stdin ())) (Unix.isatty (Fd.stdout ()))
    with
    | true, true ->
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Async_interactive.ask_yn ~default:false prompt)
    | _ ->
      Deferred.Or_error.error_s
        [%message "stdin/stdout is not a tty: pass -yes to skip the confirmation prompt"])
;;
