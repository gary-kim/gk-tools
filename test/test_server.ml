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
