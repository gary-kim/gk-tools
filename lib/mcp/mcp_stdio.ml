open! Core
open! Async
open Mcp_protocol

module Header = struct
  type t = { id : Request.Id.t option [@jsonaf.option] }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

let handle_line session line =
  match Jsonaf.parse line with
  | Error error ->
    Deferred.return
      (Some
         (Response.error
            ~id:None
            (Rpc_error.create ~code:(-32700) (Error.to_string_hum error))))
  | Ok json ->
    (match Request.of_jsonaf json with
     | Error error ->
       let id =
         Or_error.try_with (fun () -> Header.t_of_jsonaf json)
         |> Result.ok
         |> Option.bind ~f:(fun { Header.id } -> id)
       in
       Deferred.return
         (Some
            (Response.error
               ~id
               (Rpc_error.create ~code:(-32600) (Error.to_string_hum error))))
     | Ok { jsonrpc = _; id = None; method_; params } ->
       Mcp_server.Session.notify session ~method_ ~params;
       Deferred.return None
     | Ok { jsonrpc = _; id = Some id; method_; params } ->
       let%map result = Mcp_server.Session.request session ~id ~method_ ~params in
       Option.map result ~f:(function
         | Ok result -> Response.result ~id result
         | Error error -> Response.error ~id:(Some id) error))
;;

(* TODO: Handle writer-monitor failures and failed flushes. *)
let serve server ~reader ~writer =
  let session = Mcp_server.Session.create server in
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    Monitor.protect
      ~finally:(fun () ->
        Mcp_server.Session.close session;
        Deferred.unit)
      (fun () ->
        let%bind () =
          Reader.lines reader
          |> Pipe.iter_without_pushback ~f:(fun line ->
            don't_wait_for
              (let%map response = handle_line session line in
               Option.iter response ~f:(fun json ->
                 Writer.write_line writer (Jsonaf.to_string json))))
        in
        Mcp_server.Session.close session;
        Writer.flushed writer))
;;

let run server =
  serve server ~reader:(Lazy.force Reader.stdin) ~writer:(Lazy.force Writer.stdout)
;;
