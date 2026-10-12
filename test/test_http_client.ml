open! Core
open! Async

let%expect_test "HTTP error bodies drain and HEAD responses have no body" =
  let handler ~body _ request =
    let%bind _ = Cohttp_async.Body.to_string body in
    match Cohttp.Request.meth request with
    | `HEAD ->
      Cohttp_async.Server.respond_string
        ~headers:(Cohttp.Header.init_with "Content-Length" "4096")
        ""
    | _ ->
      let body =
        Pipe.create_reader ~close_on_exception:true (fun writer ->
          let%bind () = Pipe.write writer "begin " in
          let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 10) in
          Pipe.write writer "end")
      in
      Cohttp_async.Server.respond_with_pipe ~code:`Bad_request body
  in
  let%bind () =
    Test_server.with_server handler ~f:(fun port ->
      let request meth =
        Gk_tools.Http_client.request
          ~meth
          ~url:(Uri.of_string [%string "http://127.0.0.1:%{port#Int}/"])
          ~headers:(Cohttp.Header.init ())
          ~body:""
          ~timeout:(Time_ns.Span.of_int_sec 1)
      in
      let%bind error = request `GET in
      let%map head = request `HEAD in
      print_s
        [%message
          ""
            ~error_body_read:
              (Result.is_error error
               && Option.exists (Result.error error) ~f:(fun error ->
                 String.is_substring (Error.to_string_hum error) ~substring:"begin end")
               : bool)
            (head : string Or_error.t)])
  in
  [%expect {| ((error_body_read true) (head (Ok ""))) |}];
  return ()
;;
