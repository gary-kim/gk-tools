open! Core
open! Async
open Deferred.Or_error.Let_syntax

type t =
  | Literal of string
  | Env of string
  | Command of string
[@@deriving sexp]

let command_queue = lazy (Throttle.Sequencer.create ~continue_on_error:true ())

let run_command command =
  let wrapper =
    {|if ( : <"$2" >"$2" ) 2>/dev/null; then
  exec bash -c "$1" <"$2" 2>"$2"
else
  exec bash -c "$1" </dev/null
fi|}
  in
  Throttle.enqueue (force command_queue) (fun () ->
    let%bind terminal =
      Logged_process.run
        ~prog:"ps"
        ~args:[ "-o"; "tty="; "-p"; Pid.to_string (Unix.getpid ()) ]
        ()
    in
    let terminal = [%string "/dev/%{String.strip terminal}"] in
    Logged_process.run
      ~prog:"bash"
      ~args:[ "-c"; wrapper; "secret-source"; command; terminal ]
      ())
;;

let resolve t =
  let%bind value =
    match t with
    | Literal value -> return value
    | Env name ->
      Sys.getenv name
      |> Or_error.of_option_lazy_sexp
           ~error:[%lazy_message "secret environment variable is unset" (name : string)]
      |> Deferred.return
    | Command command ->
      let%map output = run_command command in
      String.rstrip output ~drop:(fun c -> Char.equal c '\r' || Char.equal c '\n')
  in
  if String.is_empty value
  then Deferred.Or_error.error_string "secret source produced an empty value"
  else return value
;;
