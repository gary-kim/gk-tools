open! Core
open! Async

let settings ~port options =
  Gk_tools.Config.Llm_completions_settings.t_of_sexp
    (Sexp.of_string
       [%string
         {|((url http://127.0.0.1:%{port#Int}/custom/v1/responses)
             (token (literal test-secret)) (model gateway-alias)
             (system_prompt "Shared LLM context") %{options})|}])
;;

let messages : Gk_tools.Llm_completions.Message.t Nonempty_list.t =
  [ { role = User; content = String.Utf8.of_string "An email" } ]
;;

let print_result result =
  print_s [%sexp (result : Gk_tools.Llm_completions.Response.t Or_error.t)]
;;

let%expect_test "POST uses configured endpoint, auth and generation options" =
  let handler ~body _ request =
    print_s [%sexp (Cohttp.Request.meth request : Cohttp.Code.meth)];
    print_endline (Uri.path (Cohttp.Request.uri request));
    print_s
      [%sexp
        (Cohttp.Header.get (Cohttp.Request.headers request) "authorization"
         : string option)];
    let%bind body = Cohttp_async.Body.to_string body in
    print_endline body;
    Cohttp_async.Server.respond_string
      ~status:(Cohttp.Code.status_of_code 299)
      {|{"id":"resp_test","object":"response","status":"completed","output":[
        {"id":"rs_test","type":"reasoning","content":null,"summary":[{"type":"summary_text","text":"An invoice"}]},
        {"id":"msg_commentary","type":"message","role":"assistant","status":"completed","phase":"commentary",
         "content":[{"type":"output_text","text":"Checking the email","annotations":[]}]},
        {"id":"msg_answer","type":"message","role":"assistant","status":"completed","phase":"final_answer",
          "content":[{"type":"output_text","text":"Rece","annotations":[]},{"type":"output_text","text":"ipts","annotations":[]}]}
      ],"usage":{"total_tokens":42}}|}
  in
  let%bind () =
    Test_server.with_server handler ~f:(fun port ->
      let%bind client =
        Gk_tools.Llm_completions.create
          (settings
             ~port
             "(reasoning_effort low) (reasoning_summary auto) (max_output_tokens 4096) \
              (temperature 0.25)")
        >>| Or_error.ok_exn
      in
      let%map result =
        Gk_tools.Llm_completions.complete
          client
          ~instructions:(String.Utf8.of_string "Choose a folder")
          messages
      in
      print_result result)
  in
  [%expect
    {|
    POST
    /custom/v1/responses
    ("Bearer test-secret")
    {"input":[{"type":"message","role":"user","content":"An email"}],"instructions":"Shared LLM context\n\nChoose a folder","model":"gateway-alias","stream":false,"store":false,"background":false,"reasoning":{"effort":"low","summary":"auto"},"max_output_tokens":4096,"temperature":0.25}
    (Ok ((content Receipts) (reasoning ("An invoice"))))
    |}];
  return ()
;;

let%expect_test "timeout covers reading the HTTP response body" =
  let release = Ivar.create () in
  let handler ~body _ _ =
    let%bind _ = Cohttp_async.Body.to_string body in
    let response_body =
      Pipe.create_reader ~close_on_exception:true (fun writer ->
        let%bind () = Ivar.read release in
        Pipe.write writer "{}")
    in
    Cohttp_async.Server.respond_with_pipe response_body
  in
  let%bind () =
    Test_server.with_server handler ~f:(fun port ->
      let%bind client =
        Gk_tools.Llm_completions.create (settings ~port "(timeout 50ms)")
        >>| Or_error.ok_exn
      in
      let%map result = Gk_tools.Llm_completions.complete client messages in
      print_result result;
      Ivar.fill_exn release ())
  in
  [%expect
    {| (Error ("LLM completion request" ("HTTP request timed out" (timeout 50ms)))) |}];
  return ()
;;
