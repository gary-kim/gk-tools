open! Core
open! Async
open Jsonaf.Export

module Query = struct
  type t = { text : string } [@@deriving jsonaf]
end

module Error_response = struct
  module Error = struct
    type t = { code : int } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { id : Mcp.Protocol.Request.Id.t option [@jsonaf.option]
    ; error : Error.t
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

let%expect_test "typed dispatch and stdio share implementations" =
  assert (
    Or_error.is_error
      (Jsonaf.parse {|{"jsonrpc":"2.0","id":null,"method":"ping"}|}
       |> Or_error.bind ~f:Mcp.Protocol.Request.of_jsonaf));
  let decodes = ref 0 in
  let cancelled = Ivar.create () in
  let tool =
    Mcp.Server.Tool.create
      ~name:"echo"
      ~input_schema:
        (Mcp.Protocol.Schema.Object.create
           ~properties:(String.Map.singleton "text" (Mcp.Protocol.Schema.create String))
           ~required:[ "text" ]
           ())
      ~query_of_jsonaf:(fun json ->
        incr decodes;
        Query.t_of_jsonaf json)
      ~response_to_result:Mcp.Protocol.Tool.Result.text
      ()
  in
  let implementation =
    Mcp.Server.Tool.implement tool (fun prefix context { Query.text } ->
      match text with
      | "wait" ->
        let%map () = Mcp.Server.Context.cancelled context in
        Ivar.fill_if_empty cancelled ();
        Ok "unused"
      | "fail" -> Deferred.Or_error.error_string "tool failed"
      | text -> Deferred.Or_error.return (prefix ^ text))
  in
  let mime_type = Mcp.Protocol.Mime_type.of_string "text/plain" |> Or_error.ok_exn in
  let resource =
    Mcp.Server.Resource.create ~uri:"test://context" ~name:"context" ~mime_type ()
  in
  let resource_implementation =
    Mcp.Server.Resource.implement resource (fun prefix _ ->
      Deferred.Or_error.return
        [ Mcp.Protocol.Resource.Contents.Text
            { uri = "test://context"; mime_type = Some mime_type; text = prefix }
        ])
  in
  let prompt =
    Mcp.Server.Prompt.create
      ~name:"greet"
      ~arguments:[ { name = "name"; description = None; required = true } ]
      ()
    |> Or_error.ok_exn
  in
  let prompt_implementation =
    Mcp.Server.Prompt.implement prompt (fun prefix _ arguments ->
      Deferred.Or_error.return
        { Mcp.Protocol.Prompt.Result.description = None
        ; messages =
            [ { role = User
              ; content =
                  Mcp.Protocol.Content.text
                    (prefix ^ Option.value (Map.find arguments "name") ~default:"")
              }
            ]
        })
  in
  let implementations =
    Mcp.Server.Implementations.create
      ~implementations:[ implementation; resource_implementation; prompt_implementation ]
    |> Or_error.ok_exn
  in
  assert (
    Or_error.is_error
      (Mcp.Server.Implementations.create
         ~implementations:[ implementation; implementation ]));
  let server =
    Mcp.Server.create
      ~info:{ name = "test"; version = "1" }
      ~implementations:(Mcp.Server.Implementations.lift implementations ~f:fst)
      ~state:("echo: ", ())
      ()
  in
  let local_prompt = Mcp.Server.Prompt.create ~name:"greet" () |> Or_error.ok_exn in
  let%bind result = Mcp.Server.Prompt.dispatch local_prompt server String.Map.empty in
  print_s [%sexp (result : Mcp.Protocol.Prompt.Result.t Or_error.t)];
  let%bind result = Mcp.Server.Tool.dispatch tool server { text = "direct" } in
  print_s [%sexp (result : string Or_error.t)];
  assert (Int.equal !decodes 0);
  let input, send = Pipe.create ()
  and output, receive = Pipe.create () in
  let%bind reader = Reader.of_pipe (Info.of_string "MCP requests") input
  and response_reader = Reader.of_pipe (Info.of_string "MCP responses") output
  and writer, `Closed_and_flushed_downstream flushed =
    Writer.of_pipe (Info.of_string "MCP responses") receive
  in
  let serving = Mcp.Stdio.serve server ~reader ~writer in
  let request line = Pipe.write send (line ^ "\n") in
  let read_response () =
    let%map response = Reader.read_line response_reader in
    match response with
    | `Ok line -> line
    | `Eof -> raise_s [%message "Unexpected EOF"]
  in
  let response () = read_response () >>| print_endline in
  let error_response ~id ~code () =
    let%map line = read_response () in
    let { Error_response.id = response_id; error = { code = response_code } } =
      Jsonaf.parse line
      |> Or_error.bind ~f:(fun json ->
        Or_error.try_with (fun () -> Error_response.t_of_jsonaf json))
      |> Or_error.ok_exn
    in
    assert (
      Option.equal
        (fun a b -> Int.equal (Mcp.Protocol.Request.Id.compare a b) 0)
        response_id
        id);
    assert (Int.equal response_code code)
  in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"client","version":"1"}}}|}
  in
  let%bind () = response () in
  let%bind () = request {|{"jsonrpc":"2.0","method":"notifications/initialized"}|} in
  let%bind () = request {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|} in
  let%bind () = response () in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":"slow","method":"tools/call","params":{"name":"echo","arguments":{"text":"wait"}}}|}
  in
  let%bind () = request {|{"jsonrpc":"2.0","id":3,"method":"ping"}|} in
  let%bind () = response () in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":"slow"}}|}
  in
  let%bind () = Ivar.read cancelled in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"echo","arguments":{"text":"wire"}}}|}
  in
  let%bind () = response () in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"echo","arguments":{"text":"fail"}}}|}
  in
  let%bind () = response () in
  let%bind () = request {|{"jsonrpc":"2.0","id":6,"method":"unknown"}|} in
  let%bind () = response () in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":7,"method":"resources/read","params":{"uri":"test://context"}}|}
  in
  let%bind () = response () in
  let%bind () =
    request
      {|{"jsonrpc":"2.0","id":8,"method":"prompts/get","params":{"name":"greet","arguments":{"name":"world"}}}|}
  in
  let%bind () = response () in
  let%bind () = request {|{"jsonrpc":"1.0","id":"invalid-version","method":"ping"}|} in
  let%bind () = error_response ~id:(Some (String "invalid-version")) ~code:(-32600) () in
  let%bind () = request {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":[]}|} in
  let%bind () = error_response ~id:(Some (Int 9)) ~code:(-32600) () in
  let%bind () = request "{" in
  let%bind () = error_response ~id:None ~code:(-32700) () in
  Pipe.close send;
  let%bind result = serving in
  Or_error.ok_exn result;
  let%bind () = Writer.close writer in
  let%bind () = flushed in
  let%bind () = Reader.close reader
  and () = Reader.close response_reader in
  [%expect
    {|
    (Error ("Missing required prompt argument" (name name)))
    (Ok "echo: direct")
    {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-11-25","capabilities":{"tools":{},"resources":{},"prompts":{}},"serverInfo":{"name":"test","version":"1"}}}
    {"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"echo","inputSchema":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}}]}}
    {"jsonrpc":"2.0","id":3,"result":{}}
    {"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"echo: wire"}],"isError":false}}
    {"jsonrpc":"2.0","id":5,"result":{"content":[{"type":"text","text":"tool failed"}],"isError":true}}
    {"jsonrpc":"2.0","id":6,"error":{"code":-32601,"message":"Unknown method: unknown"}}
    {"jsonrpc":"2.0","id":7,"result":{"contents":[{"uri":"test://context","mimeType":"text/plain","text":"echo: "}]}}
    {"jsonrpc":"2.0","id":8,"result":{"messages":[{"role":"user","content":{"type":"text","text":"echo: world"}}]}}
    |}];
  return ()
;;
