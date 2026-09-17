open! Core
open! Async

let log ~prog ~args = [%log.debug "run" (prog : string) (args : string list)]

let run ?stdin ~prog ~args () =
  log ~prog ~args;
  Process.run ?stdin ~prog ~args ()
;;

let create ~prog ~args () =
  log ~prog ~args;
  Process.create ~prog ~args ()
;;

let spawn ?env ~prog ~args () =
  log ~prog ~args;
  let%bind.Deferred () = Writer.flushed (force Writer.stdout) in
  let%bind.Deferred.Or_error pid =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Unix.fork_exec ?env ~prog ~argv:(prog :: args) ())
  in
  Unix.waitpid pid |> Deferred.map ~f:Or_error.return
;;
