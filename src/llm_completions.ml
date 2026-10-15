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

    module Item = struct
      type t =
        | Message of Message.t
        | Reasoning of Reasoning.t
      [@@deriving of_jsonaf ~capitalize:"snake_case"]

      let t_of_jsonaf = Tagged_object.of_jsonaf t_of_jsonaf
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
      ; output : Item.t list
      ; error : Failure.t option [@default None]
      ; incomplete_details : Incomplete_details.t option [@default None]
      }
    [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
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
    let open Or_error.Let_syntax in
    match (item : Wire.Item.t) with
    | Message ({ status = Completed; role = Assistant; _ } as message) ->
      let%map text = message_text message in
      (match message.phase with
       | Some Commentary -> [], []
       | None | Some Final_answer -> text, [])
    | Reasoning ({ status = None | Some Completed; _ } as reasoning) ->
      Ok ([], reasoning_text reasoning)
    | Message { status; _ } | Reasoning { status = Some status; _ } ->
      Or_error.error_s
        [%message "LLM returned an incomplete output item" (status : Wire.Item_status.t)]
  ;;

  let parse body =
    let open Or_error.Let_syntax in
    let%bind wire =
      Jsonaf.parse body
      |> Or_error.bind ~f:(fun json ->
        Or_error.try_with (fun () -> Wire.t_of_jsonaf json))
      |> Or_error.tag ~tag:"invalid LLM completion response"
    in
    let%bind wire =
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
    let%bind items = List.map wire.output ~f:item_text |> Or_error.combine_errors in
    let content = List.concat_map items ~f:fst |> Utf8.concat in
    let%map () =
      Result.ok_if_true
        (not (String.is_empty (String.strip (Utf8.to_string content))))
        ~error:(Error.of_string "LLM returned no text")
    in
    { content; reasoning = List.concat_map items ~f:snd }
  ;;
end

module Request = struct
  type t =
    { input : Message.Input.t list
    ; instructions : Utf8.t option [@jsonaf.option]
    ; model : string
    ; stream : bool
    ; store : bool
    ; background : bool
    ; reasoning : Reasoning.t option [@jsonaf.option]
    ; max_output_tokens : int option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
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

let complete t ?instructions messages =
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
  let body =
    Request.jsonaf_of_t
      { input = Nonempty_list.map messages ~f:Message.to_input |> Nonempty_list.to_list
      ; instructions
      ; model = t.parameters.model
      ; stream = false
      ; store = false
      ; background = false
      ; reasoning = t.parameters.reasoning
      ; max_output_tokens = t.parameters.max_output_tokens
      ; temperature = t.parameters.temperature
      }
    |> Jsonaf.to_string
  in
  let result =
    let%bind body =
      Http_client.request ~meth:`POST ~url:t.url ~headers ~body ~timeout:t.timeout
      |> Deferred.Or_error.tag ~tag:"LLM completion request"
    in
    Response.parse body |> Deferred.return
  in
  Deferred.Result.map_error result ~f:(fun error ->
    let text = Error.to_string_hum error in
    let redacted = redact text in
    if String.equal text redacted then error else Error.of_string redacted)
;;
