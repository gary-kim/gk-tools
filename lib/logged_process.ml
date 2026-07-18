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
