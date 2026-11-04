open! Core
open! Async

let cache_key = lazy (Re.Perl.compile_pat {|[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}|})

let () =
  Log.Global.For_testing.use_test_output
    ~map_output:(fun line -> Re.replace_string (force cache_key) ~by:"<cache-key>" line)
    ()
;;

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
    let request = Jsonaf.of_string body |> [%of_jsonaf: Llm_test_wire.Request.t] in
    String.substr_replace_all body ~pattern:request.prompt_cache_key ~with_:"<cache-key>"
    |> print_endline;
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
    {"input":[{"type":"message","role":"user","content":"An email"}],"instructions":"Shared LLM context\n\nChoose a folder","model":"gateway-alias","stream":false,"store":false,"background":false,"reasoning":{"effort":"low","summary":"auto"},"max_output_tokens":4096,"temperature":0.25,"prompt_cache_key":"<cache-key>"}
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

let%expect_test "tool rounds preserve output items and return tool errors to the model" =
  let cache_keys, cache_key_writer = Pipe.create () in
  let tool_turn =
    Jsonaf.of_string
      {|{"status":"completed","output":[
        {"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":"opaque-state"},
        {"type":"function_call","id":"fc_1","call_id":"call_1","name":"lookup","arguments":"{\"key\":1}","status":"completed"},
        {"type":"function_call","id":"fc_2","call_id":"call_2","name":"lookup","arguments":"{\"key\":2}","status":"completed"}
      ]}|}
  in
  let finish (request : Llm_test_wire.Request.t) =
    let input = request.input in
    List.drop input 1
    |> List.take _ 3
    |> List.iter ~f:(fun item -> Jsonaf.to_string item |> print_endline);
    print_s [%sexp (request.include_ : string list)];
    List.drop input 4
    |> List.map ~f:[%of_jsonaf: Llm_test_wire.Tool_output.t]
    |> List.map ~f:(fun item ->
      item.call_id, List.filter_map item.output ~f:(fun content -> content.text))
    |> [%sexp_of: (string * string list) list]
    |> print_s;
    {|{"status":"completed","output":[{"type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"Answer"}]}]}|}
  in
  let properties = String.Map.singleton "key" (Mcp.Protocol.Schema.create Integer) in
  let tool =
    Gk_tools.Llm_completions.Tool.create
      ~name:"lookup"
      ~description:"Look up a key"
      ~properties
      ~of_jsonaf:[%of_jsonaf: Llm_test_wire.Lookup_arguments.t]
      ~f:(fun arguments ->
        print_s [%sexp (arguments : Llm_test_wire.Lookup_arguments.t)];
        if Int.equal arguments.key 1
        then
          Deferred.Or_error.return
            (Nonempty_list.singleton
               (Gk_tools.Llm_completions.Tool.Content.text
                  (String.Utf8.of_string "Found")))
        else Deferred.Or_error.error_string "missing key")
  in
  let%bind () =
    Llm_test_wire.with_responses
      (List.map
         [ (fun _ -> Jsonaf.to_string tool_turn)
         ; finish
         ; (fun _ -> Jsonaf.to_string tool_turn)
         ]
         ~f:(fun respond (request : Llm_test_wire.Request.t) ->
           Pipe.write_without_pushback cache_key_writer request.prompt_cache_key;
           respond request))
      ~f:(fun port ->
        let%bind client =
          Gk_tools.Llm_completions.create (settings ~port "") >>| Or_error.ok_exn
        in
        let%bind result =
          Gk_tools.Llm_completions.complete client ~tools:[ tool ] messages
        in
        print_result result;
        let%map result =
          Gk_tools.Llm_completions.complete
            client
            ~tools:[ tool ]
            ~max_tool_rounds:0
            messages
        in
        print_result result)
  in
  Pipe.close cache_key_writer;
  let%bind keys = Pipe.to_list cache_keys in
  (match keys with
   | [ first; second; third ] ->
     print_s
       [%message
         "cache keys"
           ~reused_within_completion:(String.equal first second : bool)
           ~distinct_between_completions:(not (String.equal first third) : bool)]
   | _ -> print_s [%message "unexpected request count" ~count:(List.length keys : int)]);
  [%expect
    {|
    ((key 1))
    ((key 2))
    ("LLM tool call failed"(prompt_cache_key <cache-key>)(name lookup)(error"missing key"))
    {"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":"opaque-state"}
    {"type":"function_call","id":"fc_1","call_id":"call_1","name":"lookup","arguments":"{\"key\":1}","status":"completed"}
    {"type":"function_call","id":"fc_2","call_id":"call_2","name":"lookup","arguments":"{\"key\":2}","status":"completed"}
    (reasoning.encrypted_content)
    ((call_1 (Found)) (call_2 ("Tool error: missing key")))
    (Ok ((content Answer) (reasoning ())))
    (Error ("LLM tool round limit exceeded" (max_tool_rounds 0)))
    ("cache keys" (reused_within_completion true)
     (distinct_between_completions true))
    |}];
  return ()
;;
