open! Core
open! Async
open Deferred.Or_error.Let_syntax
open Jsonaf.Export

module Utf8 = struct
  include String.Utf8
  include functor Jsonaf.Jsonafable.Of_stringable
end

module Reasoning = struct
  module Effort = struct
    include Config.Llm_completions_settings.Reasoning_effort
    include functor Jsonaf.Jsonafable.Of_stringable
  end

  module Summary = struct
    include Config.Llm_completions_settings.Reasoning_summary
    include functor Jsonaf.Jsonafable.Of_stringable
  end

  type t =
    { effort : Effort.t option [@jsonaf.option]
    ; summary : Summary.t option [@jsonaf.option]
    }
  [@@deriving jsonaf_of]
end

module Message = struct
  module Role = struct
    type t =
      | User
      | Assistant
      | System
      | Developer
    [@@deriving sexp, string ~capitalize:"snake_case"]

    include functor Jsonaf.Jsonafable.Of_stringable
  end

  type t =
    { role : Role.t
    ; content : Utf8.t
    }

  module Input = struct
    module Kind = struct
      type t = Message [@@deriving string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    type t =
      { type_ : Kind.t [@key "type"]
      ; role : Role.t
      ; content : Utf8.t
      }
    [@@deriving jsonaf_of]
  end

  let to_input { role; content } : Input.t = { type_ = Message; role; content }
end

module Tagged_object = struct
  module Tag = struct
    type t = { type_ : string [@key "type"] }
    [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
  end

  let of_jsonaf convert json =
    let { Tag.type_ } = Tag.t_of_jsonaf json in
    convert (`Array [ `String type_; json ])
  ;;
end

module Tool = struct
  module Content = struct
    module Text = struct
      module Kind = struct
        type t = Input_text [@@deriving string ~capitalize:"snake_case"]

        include functor Jsonaf.Jsonafable.Of_stringable
      end

      type t =
        { type_ : Kind.t [@key "type"]
        ; text : Utf8.t
        }
      [@@deriving jsonaf_of]
    end

    module Image = struct
      module Kind = struct
        type t = Input_image [@@deriving string ~capitalize:"snake_case"]

        include functor Jsonaf.Jsonafable.Of_stringable
      end

      type t =
        { type_ : Kind.t [@key "type"]
        ; image_url : string
        }
      [@@deriving jsonaf_of]
    end

    module File = struct
      module Kind = struct
        type t = Input_file [@@deriving string ~capitalize:"snake_case"]

        include functor Jsonaf.Jsonafable.Of_stringable
      end

      type t =
        { type_ : Kind.t [@key "type"]
        ; filename : string
        ; file_data : string
        }
      [@@deriving jsonaf_of]
    end

    type t =
      | Text of Text.t
      | Image of Image.t
      | File of File.t

    let jsonaf_of_t t =
      match t with
      | Text text -> [%jsonaf_of: Text.t] text
      | Image image -> [%jsonaf_of: Image.t] image
      | File file -> [%jsonaf_of: File.t] file
    ;;

    let text text = Text { type_ = Input_text; text }

    let data_url ~mime_type data =
      let mime_type = Mcp.Protocol.Mime_type.to_string mime_type in
      let encoded = Base64.encode_string data in
      [%string "data:%{mime_type};base64,%{encoded}"]
    ;;

    let image ~mime_type ~data =
      Image { type_ = Input_image; image_url = data_url ~mime_type data }
    ;;

    let file ~filename ~mime_type ~data =
      File { type_ = Input_file; filename; file_data = data_url ~mime_type data }
    ;;
  end

  module Definition = struct
    module Kind = struct
      type t = Function [@@deriving string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    type t =
      { type_ : Kind.t [@key "type"]
      ; name : string
      ; description : string
      ; parameters : Mcp.Protocol.Schema.Object.t
      ; strict : bool
      }
    [@@deriving jsonaf_of]
  end

  module Output = struct
    module Kind = struct
      type t = Function_call_output [@@deriving string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    type t =
      { type_ : Kind.t [@key "type"]
      ; call_id : string
      ; output : Content.t list
      }
    [@@deriving jsonaf_of]
  end

  type t =
    { definition : Definition.t
    ; call : Jsonaf.t -> Content.t Nonempty_list.t Deferred.Or_error.t
    }

  let create ~name ~description ~properties ~of_jsonaf ~f =
    let parameters =
      Mcp.Protocol.Schema.Object.create
        ~properties
        ~required:(Map.keys properties)
        ~additional_properties:false
        ()
    in
    { definition = { type_ = Function; name; description; parameters; strict = true }
    ; call =
        (fun json ->
          let%bind arguments =
            Or_error.try_with (fun () -> of_jsonaf json) |> Deferred.return
          in
          f arguments)
    }
  ;;
end

module Response = struct
  type t =
    { content : Utf8.t
    ; reasoning : Utf8.t list
    }
  [@@deriving sexp_of]

  module Wire = struct
    module Text = struct
      type t = { text : Utf8.t } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    module Refusal = struct
      type t = { refusal : Utf8.t } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    module Message_content = struct
      type t =
        | Output_text of Text.t
        | Refusal of Refusal.t
      [@@deriving of_jsonaf ~capitalize:"snake_case"]

      let t_of_jsonaf = Tagged_object.of_jsonaf t_of_jsonaf
    end

    module Reasoning_content = struct
      type t =
        | Summary_text of Text.t
        | Reasoning_text of Text.t
        | Output_text of Text.t
        | Text of Text.t
      [@@deriving of_jsonaf ~capitalize:"snake_case"]

      let t_of_jsonaf = Tagged_object.of_jsonaf t_of_jsonaf
    end

    module Item_status = struct
      type t =
        | Completed
        | In_progress
        | Incomplete
      [@@deriving sexp_of, string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    module Message = struct
      module Role = struct
        type t = Assistant [@@deriving string ~capitalize:"snake_case"]

        include functor Jsonaf.Jsonafable.Of_stringable
      end

      module Phase = struct
        type t =
          | Commentary
          | Final_answer
        [@@deriving string ~capitalize:"snake_case"]

        include functor Jsonaf.Jsonafable.Of_stringable
      end

      type t =
        { status : Item_status.t
        ; role : Role.t
        ; phase : Phase.t option [@default None]
        ; content : Message_content.t list
        }
      [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    module Reasoning = struct
      type t =
        { status : Item_status.t option [@default None]
        ; summary : Reasoning_content.t list
        ; content : Reasoning_content.t list option [@default None]
        }
      [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    module Function_call = struct
      type t =
        { call_id : string
        ; name : string
        ; arguments : string
        ; status : Item_status.t option [@default None]
        }
      [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    module Item = struct
      type t =
        | Message of Message.t
        | Reasoning of Reasoning.t
        | Function_call of Function_call.t
      [@@deriving of_jsonaf ~capitalize:"snake_case"]

      let t_of_jsonaf = Tagged_object.of_jsonaf t_of_jsonaf
    end

    module Output_item = struct
      type t =
        { item : Item.t
        ; original : Jsonaf.t
        }

      let t_of_jsonaf original = { item = [%of_jsonaf: Item.t] original; original }
      let jsonaf_of_t t = t.original
    end

    module Status = struct
      type t =
        | Completed
        | Incomplete
        | Failed
        | In_progress
        | Queued
        | Cancelled
      [@@deriving sexp_of, string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    module Failure = struct
      type t =
        { code : string
        ; message : string
        }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    module Incomplete_details = struct
      type t = { reason : string }
      [@@deriving of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]
    end

    type t =
      { status : Status.t
      ; output : Output_item.t list
      ; error : Failure.t option [@default None]
      ; incomplete_details : Incomplete_details.t option [@default None]
      }
    [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
  end

  module Parsed = struct
    type nonrec t =
      { output : Wire.Output_item.t list
      ; tool_calls : Wire.Function_call.t list
      ; response : t
      }
  end

  let message_text (message : Wire.Message.t) =
    List.map message.content ~f:(fun part ->
      match (part : Wire.Message_content.t) with
      | Output_text { text } -> Ok text
      | Refusal { refusal } ->
        Or_error.error_s [%message "LLM refused to answer" (refusal : Utf8.t)])
    |> Or_error.combine_errors
  ;;

  let reasoning_text (reasoning : Wire.Reasoning.t) =
    (if List.is_empty reasoning.summary
     then Option.value reasoning.content ~default:[]
     else reasoning.summary)
    |> List.map ~f:(fun part ->
      match (part : Wire.Reasoning_content.t) with
      | Summary_text { text }
      | Reasoning_text { text }
      | Output_text { text }
      | Text { text } -> text)
  ;;

  let item_text item =
    match (item : Wire.Item.t) with
    | Message ({ status = Completed; role = Assistant; _ } as message) ->
      let%map.Or_error text = message_text message in
      (match message.phase with
       | Some Commentary -> [], []
       | None | Some Final_answer -> text, [])
    | Reasoning ({ status = None | Some Completed; _ } as reasoning) ->
      Ok ([], reasoning_text reasoning)
    | Function_call { status = None | Some Completed; _ } -> Ok ([], [])
    | Message { status; _ }
    | Reasoning { status = Some status; _ }
    | Function_call { status = Some status; _ } ->
      Or_error.error_s
        [%message "LLM returned an incomplete output item" (status : Wire.Item_status.t)]
  ;;

  let parse body =
    let%bind.Or_error wire =
      Jsonaf.parse body
      |> Or_error.bind ~f:(fun json ->
        Or_error.try_with (fun () -> Wire.t_of_jsonaf json))
      |> Or_error.tag ~tag:"invalid LLM completion response"
    in
    let%bind.Or_error wire =
      match wire.status, wire.error, wire.incomplete_details with
      | Completed, None, None -> Ok wire
      | _ ->
        Or_error.error_s
          [%message
            "LLM did not return a complete text answer"
              ~status:(wire.status : Wire.Status.t)
              ~error:(wire.error : Wire.Failure.t option)
              ~incomplete_details:
                (wire.incomplete_details : Wire.Incomplete_details.t option)]
    in
    let items = List.map wire.output ~f:(fun item -> item.Wire.Output_item.item) in
    let%map.Or_error text = List.map items ~f:item_text |> Or_error.combine_errors in
    { Parsed.output = wire.output
    ; tool_calls =
        List.filter_map items ~f:(fun item ->
          match (item : Wire.Item.t) with
          | Function_call call -> Some call
          | Message _ | Reasoning _ -> None)
    ; response =
        { content = List.concat_map text ~f:fst |> Utf8.concat
        ; reasoning = List.concat_map text ~f:snd
        }
    }
  ;;

  let finish t =
    let%map.Or_error () =
      Result.ok_if_true
        (not (String.is_empty (String.strip (Utf8.to_string t.content))))
        ~error:(Error.of_string "LLM returned no text")
    in
    t
  ;;
end

module Request = struct
  module Input = struct
    type t =
      | Message of Message.Input.t
      | Output_item of Response.Wire.Output_item.t
      | Tool_output of Tool.Output.t

    let jsonaf_of_t t =
      match t with
      | Message message -> [%jsonaf_of: Message.Input.t] message
      | Output_item item -> [%jsonaf_of: Response.Wire.Output_item.t] item
      | Tool_output output -> [%jsonaf_of: Tool.Output.t] output
    ;;
  end

  module Include = struct
    type t = Reasoning_encrypted_content [@rename "reasoning.encrypted_content"]
    [@@deriving string]

    include functor Jsonaf.Jsonafable.Of_stringable
  end

  type t =
    { input : Input.t list
    ; instructions : Utf8.t option [@jsonaf.option]
    ; model : string
    ; stream : bool
    ; store : bool
    ; background : bool
    ; reasoning : Reasoning.t option [@jsonaf.option]
    ; max_output_tokens : int option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; tools : Tool.Definition.t list option [@jsonaf.option]
    ; include_ : Include.t list option [@key "include"] [@jsonaf.option]
    ; prompt_cache_key : string
    }
  [@@deriving jsonaf_of]
end

module Parameters = struct
  type t =
    { model : string
    ; reasoning : Reasoning.t option
    ; max_output_tokens : int option
    ; temperature : float option
    }
end

type t =
  { url : Uri.t
  ; token : string option
  ; timeout : Time_ns.Span.t
  ; system_prompt : Utf8.t option
  ; parameters : Parameters.t
  }

let resolve_token (settings : Config.Llm_completions_settings.t) =
  Option.value_map settings.token ~default:(return None) ~f:(fun source ->
    let%bind token =
      Secret_source.resolve source |> Deferred.Or_error.tag ~tag:"LLM token"
    in
    let token = String.strip token in
    let%map () =
      Result.ok_if_true
        ((not (String.is_empty token))
         && String.for_all token ~f:(fun c ->
           Char.is_print c && not (Char.is_whitespace c)))
        ~error:
          (Error.of_string
             "LLM token must be nonempty printable ASCII and contain no whitespace")
      |> Deferred.return
    in
    Some token)
;;

let create (settings : Config.Llm_completions_settings.t) =
  let reasoning =
    Option.some_if
      (Option.is_some settings.reasoning_effort
       || Option.is_some settings.reasoning_summary)
      { Reasoning.effort = settings.reasoning_effort
      ; summary = settings.reasoning_summary
      }
  in
  let%map token = resolve_token settings in
  { url = Config.Llm_completions_settings.Url.to_uri settings.url
  ; token
  ; timeout = Config.Llm_completions_settings.Timeout.raw settings.timeout
  ; system_prompt = settings.system_prompt
  ; parameters =
      { model = Config.Llm_completions_settings.Model.to_string settings.model
      ; reasoning
      ; max_output_tokens =
          Option.map
            settings.max_output_tokens
            ~f:Config.Llm_completions_settings.Max_output_tokens.raw
      ; temperature =
          Option.map
            settings.temperature
            ~f:Config.Llm_completions_settings.Temperature.raw
      }
  }
;;

let complete t ?instructions ?tools ?(max_tool_rounds = 8) messages =
  let prompt_cache_key = Uuid_unix.create () |> Uuid.to_string in
  let instructions =
    Option.merge t.system_prompt instructions ~f:(fun system_prompt instructions ->
      Utf8.concat [ system_prompt; Utf8.of_string "\n\n"; instructions ])
  in
  let redact =
    Option.value_map t.token ~default:Fn.id ~f:(fun token ->
      String.substr_replace_all ~pattern:token ~with_:"[REDACTED]")
  in
  let headers =
    let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
    Option.value_map t.token ~default:headers ~f:(fun token ->
      Cohttp.Header.add headers "Authorization" [%string "Bearer %{token}"])
  in
  let request input =
    let body =
      [%jsonaf_of: Request.t]
        { input
        ; instructions
        ; model = t.parameters.model
        ; stream = false
        ; store = false
        ; background = false
        ; reasoning = t.parameters.reasoning
        ; max_output_tokens = t.parameters.max_output_tokens
        ; temperature = t.parameters.temperature
        ; tools =
            Option.map tools ~f:(fun tools ->
              Nonempty_list.map tools ~f:(fun tool -> tool.Tool.definition)
              |> Nonempty_list.to_list)
        ; include_ =
            Option.map tools ~f:(fun (_ : Tool.t Nonempty_list.t) ->
              [ Request.Include.Reasoning_encrypted_content ])
        ; prompt_cache_key
        }
      |> Jsonaf.to_string
    in
    let%bind body =
      Http_client.request ~meth:`POST ~url:t.url ~headers ~body ~timeout:t.timeout
      |> Deferred.Or_error.tag ~tag:"LLM completion request"
    in
    Response.parse body |> Deferred.return
  in
  let call_tool (call : Response.Wire.Function_call.t) =
    [%log.debug
      "LLM tool call"
        (prompt_cache_key : string)
        ~name:(call.name : string)
        ~arguments:(call.arguments : string)];
    let%map.Deferred result =
      let%bind tool =
        Option.bind
          tools
          ~f:
            (Nonempty_list.find ~f:(fun tool ->
               String.equal tool.Tool.definition.name call.name))
        |> Or_error.of_option_lazy_sexp
             ~error:[%lazy_message "unknown LLM tool" ~name:(call.name : string)]
        |> Deferred.return
      in
      let%bind arguments = Jsonaf.parse call.arguments |> Deferred.return in
      Deferred.Or_error.try_with_join ~extract_exn:true (fun () -> tool.call arguments)
    in
    let output =
      match result with
      | Ok content -> Nonempty_list.to_list content
      | Error error ->
        [%log.info
          "LLM tool call failed"
            (prompt_cache_key : string)
            ~name:(call.name : string)
            (error : Error.t)];
        let message = Error.to_string_hum error |> redact in
        [ Tool.Content.text (String.Utf8.sanitize [%string "Tool error: %{message}"]) ]
    in
    Request.Input.Tool_output
      { type_ = Function_call_output; call_id = call.call_id; output }
  in
  let result =
    let%bind () =
      Option.bind tools ~f:(fun tools ->
        Nonempty_list.map tools ~f:(fun tool -> tool.Tool.definition.name)
        |> Nonempty_list.to_list
        |> List.find_a_dup ~compare:[%compare: string])
      |> Option.value_map ~default:(return ()) ~f:(fun name ->
        Deferred.Or_error.error_s [%message "duplicate LLM tool" (name : string)])
    in
    let input =
      Nonempty_list.map messages ~f:(fun message ->
        Request.Input.Message (Message.to_input message))
      |> Nonempty_list.to_list
    in
    Deferred.Or_error.repeat_until_finished
      (input, 0, [])
      (fun (input, rounds, reasoning) ->
         let%bind parsed = request input in
         let reasoning = List.rev_append parsed.response.reasoning reasoning in
         match parsed.tool_calls with
         | [] ->
           Response.finish { parsed.response with reasoning = List.rev reasoning }
           |> Or_error.map ~f:(fun response -> `Finished response)
           |> Deferred.return
         | calls ->
           let%bind () =
             Result.ok_if_true
               (rounds < max_tool_rounds)
               ~error:
                 (Error.create_s
                    [%message "LLM tool round limit exceeded" (max_tool_rounds : int)])
             |> Deferred.return
           in
           let%map.Deferred output =
             Deferred.List.map calls ~how:`Sequential ~f:call_tool
           in
           let history =
             List.map parsed.output ~f:(fun item -> Request.Input.Output_item item)
           in
           Ok (`Repeat (input @ history @ output, rounds + 1, reasoning)))
  in
  Deferred.Result.map_error result ~f:(fun error ->
    let text = Error.to_string_hum error in
    let redacted = redact text in
    if String.equal text redacted then error else Error.of_string redacted)
;;
