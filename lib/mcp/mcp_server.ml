open! Core
open! Async
open Mcp_protocol

module Context = struct
  type t = { cancelled : unit Ivar.t }

  let create () = { cancelled = Ivar.create () }
  let cancelled t = Ivar.read t.cancelled
  let is_cancelled t = Ivar.is_full t.cancelled
  let cancel t = Ivar.fill_if_empty t.cancelled ()
end

module Description = struct
  module Tool = struct
    type ('query, 'response) t =
      { definition : Mcp_protocol.Tool.t
      ; id : ('query -> 'response) Type_equal.Id.t
      ; query_of_jsonaf : Jsonaf.t -> 'query
      ; response_to_result : 'response -> Mcp_protocol.Tool.Result.t
      }
  end

  module Resource = struct
    type t = Mcp_protocol.Resource.t
  end

  module Prompt = struct
    type t = Mcp_protocol.Prompt.t
  end
end

module Implementation = struct
  module Tool = struct
    type 'state t =
      | T :
          ('query, 'response) Description.Tool.t
          * ('state -> Context.t -> 'query -> 'response Deferred.Or_error.t)
          -> 'state t

    let lift (T (description, handler)) ~f =
      T (description, fun state -> handler (f state))
    ;;
  end

  module Resource = struct
    type 'state t =
      { description : Description.Resource.t
      ; handler :
          'state -> Context.t -> Mcp_protocol.Resource.Contents.t list Deferred.Or_error.t
      }

    let lift { description; handler } ~f =
      { description; handler = (fun state -> handler (f state)) }
    ;;
  end

  module Prompt = struct
    type 'state t =
      { description : Description.Prompt.t
      ; handler :
          'state
          -> Context.t
          -> string String.Map.t
          -> Mcp_protocol.Prompt.Result.t Deferred.Or_error.t
      }

    let lift { description; handler } ~f =
      { description; handler = (fun state -> handler (f state)) }
    ;;
  end

  type 'state t =
    | Tool of 'state Tool.t
    | Resource of 'state Resource.t
    | Prompt of 'state Prompt.t

  let lift t ~f =
    match t with
    | Tool implementation -> Tool (Tool.lift implementation ~f)
    | Resource implementation -> Resource (Resource.lift implementation ~f)
    | Prompt implementation -> Prompt (Prompt.lift implementation ~f)
  ;;
end

open Implementation

module Implementations = struct
  type 'state t =
    { tools : 'state Implementation.Tool.t String.Map.t
    ; resources : 'state Implementation.Resource.t String.Map.t
    ; prompts : 'state Implementation.Prompt.t String.Map.t
    }

  let create ~implementations =
    let open Or_error.Let_syntax in
    let registry select =
      List.filter_map implementations ~f:select |> Map.of_alist_or_error (module String)
    in
    let%map tools =
      registry (function
        | Tool (T (description, _) as implementation) ->
          Some (description.definition.name, implementation)
        | Resource _ | Prompt _ -> None)
    and resources =
      registry (function
        | Resource ({ description; handler = _ } as implementation) ->
          Some (description.uri, implementation)
        | Tool _ | Prompt _ -> None)
    and prompts =
      registry (function
        | Prompt ({ description; handler = _ } as implementation) ->
          Some (description.name, implementation)
        | Tool _ | Resource _ -> None)
    in
    { tools; resources; prompts }
  ;;

  let lift t ~f =
    { tools =
        Map.map t.tools ~f:(fun implementation ->
          Implementation.Tool.lift implementation ~f)
    ; resources =
        Map.map t.resources ~f:(fun implementation ->
          Implementation.Resource.lift implementation ~f)
    ; prompts =
        Map.map t.prompts ~f:(fun implementation ->
          Implementation.Prompt.lift implementation ~f)
    }
  ;;
end

type 'state t =
  { info : Mcp_protocol.Implementation.t
  ; instructions : string option
  ; implementations : 'state Implementations.t
  ; state : 'state
  }

let create ~info ?instructions ~implementations ~state () =
  { info; instructions; implementations; state }
;;

let protect f = Deferred.Or_error.try_with ~extract_exn:true f >>| Or_error.join

let protect_handler context f =
  if Context.is_cancelled context
  then Deferred.Or_error.error_string "Request cancelled"
  else protect f
;;

module Tool = struct
  type ('query, 'response) t = ('query, 'response) Description.Tool.t

  let create
    ~name
    ?description
    ~input_schema
    ?output_schema
    ~query_of_jsonaf
    ~response_to_result
    ()
    =
    { Description.Tool.definition = { name; description; input_schema; output_schema }
    ; id = Type_equal.Id.create ~name [%sexp_of: opaque]
    ; query_of_jsonaf
    ; response_to_result
    }
  ;;

  let definition t = t.Description.Tool.definition
  let implement t handler = Implementation.Tool (T (t, handler))

  let dispatch
    (type query response)
    (description : (query, response) t)
    server
    ?(context = Context.create ())
    (query : query)
    : response Deferred.Or_error.t
    =
    match Map.find server.implementations.tools description.definition.name with
    | Some (T (implemented, handler)) ->
      (match Type_equal.Id.same_witness description.id implemented.id with
       | Some T -> protect_handler context (fun () -> handler server.state context query)
       | None ->
         Deferred.Or_error.error_string
           "Tool description differs from the implemented description")
    | None ->
      Deferred.Or_error.error_s
        [%message "Tool is not implemented" ~name:(description.definition.name : string)]
  ;;
end

module Resource = struct
  type t = Description.Resource.t

  let create ~uri ~name ?description ?mime_type () : t =
    { uri; name; description; mime_type }
  ;;

  let definition = Fn.id
  let implement t handler = Implementation.Resource { description = t; handler }

  let dispatch (description : t) server ?(context = Context.create ()) () =
    match Map.find server.implementations.resources description.uri with
    | Some { description = _; handler } ->
      protect_handler context (fun () -> handler server.state context)
    | None ->
      Deferred.Or_error.error_s
        [%message "Resource is not implemented" ~uri:(description.uri : string)]
  ;;
end

module Prompt = struct
  type t = Description.Prompt.t

  let create ~name ?description ?(arguments = []) () : t Or_error.t =
    let open Or_error.Let_syntax in
    let%map () =
      List.find_a_dup arguments ~compare:(fun (a : Mcp_protocol.Prompt.Argument.t) b ->
        String.compare a.name b.name)
      |> Option.value_map ~default:(Ok ()) ~f:(fun argument ->
        Or_error.error_s
          [%message "Duplicate prompt argument name" ~name:(argument.name : string)])
    in
    ({ name; description; arguments } : t)
  ;;

  let definition = Fn.id
  let implement t handler = Implementation.Prompt { description = t; handler }

  let validate_arguments (description : t) arguments =
    let open Or_error.Let_syntax in
    let%map () =
      List.filter
        description.arguments
        ~f:(fun (argument : Mcp_protocol.Prompt.Argument.t) ->
          argument.required && not (Map.mem arguments argument.name))
      |> List.map ~f:(fun (argument : Mcp_protocol.Prompt.Argument.t) ->
        Or_error.error_s
          [%message "Missing required prompt argument" ~name:(argument.name : string)])
      |> Or_error.combine_errors_unit
    in
    arguments
  ;;

  let dispatch (description : t) server ?(context = Context.create ()) arguments =
    let open Deferred.Or_error.Let_syntax in
    match Map.find server.implementations.prompts description.name with
    | Some { description; handler } ->
      let%bind arguments = validate_arguments description arguments |> Deferred.return in
      protect_handler context (fun () -> handler server.state context arguments)
    | None ->
      Deferred.Or_error.error_s
        [%message "Prompt is not implemented" ~name:(description.name : string)]
  ;;
end

let list_tools t =
  Map.data t.implementations.tools
  |> List.map ~f:(fun (T (description, _)) -> description.definition)
;;

let list_resources t =
  Map.data t.implementations.resources
  |> List.map ~f:(fun { Implementation.Resource.description; handler = _ } -> description)
;;

let list_prompts t =
  Map.data t.implementations.prompts
  |> List.map ~f:(fun { Implementation.Prompt.description; handler = _ } -> description)
;;

let parse_params convert params =
  Or_error.try_with (fun () ->
    Option.value params ~default:String.Map.empty
    |> Json_object.jsonaf_of_t Jsonaf.jsonaf_of_t
    |> convert)
  |> Result.map_error ~f:(fun error ->
    Rpc_error.invalid_params (Error.to_string_hum error))
;;

let call_tool t ?(context = Context.create ()) ~name ~arguments () =
  match Map.find t.implementations.tools name with
  | Some (T (description, handler)) ->
    let computation =
      let open Deferred.Or_error.Let_syntax in
      let%bind query =
        Or_error.try_with (fun () ->
          Json_object.jsonaf_of_t Jsonaf.jsonaf_of_t arguments
          |> description.query_of_jsonaf)
        |> Deferred.return
      in
      let%bind response =
        protect_handler context (fun () -> handler t.state context query)
      in
      Or_error.try_with (fun () -> description.response_to_result response)
      |> Deferred.return
    in
    let%map result = computation in
    Ok
      (match result with
       | Ok result -> result
       | Error error ->
         Mcp_protocol.Tool.Result.text ~is_error:true (Error.to_string_hum error))
  | None ->
    Deferred.return (Error (Rpc_error.invalid_params [%string "Unknown tool: %{name}"]))
;;

let dispatch t ~context ~method_ ~params =
  let open Deferred.Result.Let_syntax in
  let parse convert = Deferred.return (parse_params convert params) in
  let list result =
    let%bind { Mcp_wire.List_params.cursor } = parse Mcp_wire.List_params.t_of_jsonaf in
    let%map () =
      Result.ok_if_true
        (Option.is_none cursor)
        ~error:(Rpc_error.invalid_params "This server does not issue pagination cursors")
      |> Deferred.return
    in
    result
  in
  match method_ with
  | "ping" -> Deferred.return (Ok (Mcp_wire.Empty.jsonaf_of_t ()))
  | "tools/list" when not (Map.is_empty t.implementations.tools) ->
    list (Mcp_wire.Tools.jsonaf_of_t { tools = list_tools t })
  | "tools/call" when not (Map.is_empty t.implementations.tools) ->
    let%bind { Mcp_wire.Tool_call.name; arguments; task } =
      parse Mcp_wire.Tool_call.t_of_jsonaf
    in
    let%bind () =
      Result.ok_if_true
        (Option.is_none task)
        ~error:(Rpc_error.invalid_params "Task-augmented execution is not supported")
      |> Deferred.return
    in
    let%map result = call_tool t ~context ~name ~arguments () in
    Mcp_protocol.Tool.Result.jsonaf_of_t result
  | "resources/list" when not (Map.is_empty t.implementations.resources) ->
    list (Mcp_wire.Resources.jsonaf_of_t { resources = list_resources t })
  | "resources/templates/list" when not (Map.is_empty t.implementations.resources) ->
    list (Mcp_wire.Resource_templates.jsonaf_of_t { resource_templates = [] })
  | "resources/read" when not (Map.is_empty t.implementations.resources) ->
    let%bind { Mcp_wire.Resource_read.uri } = parse Mcp_wire.Resource_read.t_of_jsonaf in
    (match Map.find t.implementations.resources uri with
     | Some { description; handler = _ } ->
       let%map contents =
         Deferred.Result.map_error
           (Resource.dispatch description t ~context ())
           ~f:Rpc_error.internal
       in
       Mcp_wire.Contents.jsonaf_of_t { contents }
     | None ->
       Deferred.return
         (Error (Rpc_error.create ~code:(-32002) [%string "Unknown resource: %{uri}"])))
  | "prompts/list" when not (Map.is_empty t.implementations.prompts) ->
    list (Mcp_wire.Prompts.jsonaf_of_t { prompts = list_prompts t })
  | "prompts/get" when not (Map.is_empty t.implementations.prompts) ->
    let%bind { Mcp_wire.Prompt_get.name; arguments } =
      parse Mcp_wire.Prompt_get.t_of_jsonaf
    in
    (match Map.find t.implementations.prompts name with
     | Some { description; handler } ->
       let%bind arguments =
         Prompt.validate_arguments description arguments
         |> Result.map_error ~f:(fun error ->
           Rpc_error.invalid_params (Error.to_string_hum error))
         |> Deferred.return
       in
       let%map result =
         Deferred.Result.map_error
           (protect_handler context (fun () -> handler t.state context arguments))
           ~f:Rpc_error.internal
       in
       Mcp_protocol.Prompt.Result.jsonaf_of_t result
     | None ->
       Deferred.return
         (Error (Rpc_error.invalid_params [%string "Unknown prompt: %{name}"])))
  | _ ->
    Deferred.return
      (Error (Rpc_error.create ~code:(-32601) [%string "Unknown method: %{method_}"]))
;;

module Session = struct
  module State = struct
    type t =
      | New
      | Initializing
      | Ready
      | Closed
  end

  type nonrec 'state t =
    { server : 'state t
    ; mutable state : State.t
    ; pending : Context.t Hashtbl.M(Request.Id).t
    }

  let create server =
    { server; state = New; pending = Hashtbl.create (module Request.Id) }
  ;;

  (* TODO: Allow callers to wait for in-flight handlers after closing. *)
  let close t =
    t.state <- Closed;
    Hashtbl.iter t.pending ~f:Context.cancel
  ;;

  let notify t ~method_ ~params =
    match method_, t.state with
    | "notifications/initialized", Initializing -> t.state <- Ready
    | "notifications/cancelled", _ ->
      parse_params Mcp_wire.Cancelled.t_of_jsonaf params
      |> Result.ok
      |> Option.bind ~f:(fun cancelled -> Hashtbl.find t.pending cancelled.request_id)
      |> Option.iter ~f:Context.cancel
    | _ -> ()
  ;;

  let initialize t params =
    let open Result.Let_syntax in
    let%bind { Mcp_wire.Initialize.protocol_version = requested_version
             ; capabilities
             ; client_info
             }
      =
      parse_params Mcp_wire.Initialize.t_of_jsonaf params
    in
    ignore (capabilities : Jsonaf.t Json_object.t);
    ignore (client_info : Mcp_protocol.Implementation.t);
    let server = t.server in
    let implementations = server.implementations in
    let capabilities : Mcp_wire.Capabilities.t =
      { tools = Option.some_if (not (Map.is_empty implementations.tools)) ()
      ; resources = Option.some_if (not (Map.is_empty implementations.resources)) ()
      ; prompts = Option.some_if (not (Map.is_empty implementations.prompts)) ()
      }
    in
    (* TODO: Enforce version-specific response formats. *)
    let version =
      if List.mem
           [ protocol_version; "2025-06-18"; "2024-11-05" ]
           requested_version
           ~equal:String.equal
      then requested_version
      else protocol_version
    in
    t.state <- Initializing;
    Ok
      (Mcp_wire.Initialized.jsonaf_of_t
         { protocol_version = version
         ; capabilities
         ; server_info = server.info
         ; instructions = server.instructions
         })
  ;;

  let request t ~id ~method_ ~params =
    let immediate result = Deferred.return (Some result) in
    match method_, t.state with
    | _, Closed -> Deferred.return None
    | "initialize", New -> immediate (initialize t params)
    | "initialize", _ ->
      immediate (Error (Rpc_error.invalid_params "Already initialized"))
    | "ping", _ -> immediate (Ok (Mcp_wire.Empty.jsonaf_of_t ()))
    | _, (New | Initializing) ->
      immediate (Error (Rpc_error.create ~code:(-32000) "Server is not initialized"))
    | _, Ready ->
      let context = Context.create () in
      (match Hashtbl.add t.pending ~key:id ~data:context with
       | `Duplicate ->
         immediate (Error (Rpc_error.invalid_params "Request ID is already in flight"))
       | `Ok ->
         let%map result =
           Deferred.choose
             [ Deferred.choice (dispatch t.server ~context ~method_ ~params) Option.some
             ; Deferred.choice (Context.cancelled context) (fun () -> None)
             ]
         in
         Hashtbl.remove t.pending id;
         if Context.is_cancelled context then None else result)
  ;;
end
