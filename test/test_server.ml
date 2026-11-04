open! Core
open! Async

let with_server handler ~f =
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      handler
  in
  Monitor.protect
    ~finally:(fun () -> Cohttp_async.Server.close server)
    (fun () -> f (Cohttp_async.Server.listening_on server))
;;

let with_handlers handlers ~f =
  let handlers = Pipe.of_list handlers in
  let handler ~body socket request =
    match%bind Pipe.read handlers with
    | `Ok handler -> handler ~body socket request
    | `Eof -> raise_s [%message "unexpected HTTP request"]
  in
  let%bind result = with_server handler ~f in
  match%map Pipe.read handlers with
  | `Eof -> result
  | `Ok _ -> raise_s [%message "expected another HTTP request"]
;;
