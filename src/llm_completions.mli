open! Core
open! Async

module Message : sig
  module Role : sig
    type t =
      | User
      | Assistant
      | System
      | Developer
    [@@deriving sexp]
  end

  type t =
    { role : Role.t
    ; content : String.Utf8.t
    }
end

module Tool : sig
  module Content : sig
    type t

    val text : String.Utf8.t -> t
    val image : mime_type:Mcp.Protocol.Mime_type.t -> data:string -> t
    val file : filename:string -> mime_type:Mcp.Protocol.Mime_type.t -> data:string -> t
  end

  type t

  val create
    :  name:string
    -> description:string
    -> properties:Mcp.Protocol.Schema.t Mcp.Protocol.Json_object.t
    -> of_jsonaf:(Jsonaf.t -> 'a)
    -> f:('a -> Content.t Nonempty_list.t Deferred.Or_error.t)
    -> t
end

module Response : sig
  type t =
    { content : String.Utf8.t
    ; reasoning : String.Utf8.t list
    }
  [@@deriving sexp_of]
end

type t

val create : Config.Llm_completions_settings.t -> t Deferred.Or_error.t

val complete
  :  t
  -> ?instructions:String.Utf8.t
  -> ?tools:Tool.t Nonempty_list.t
  -> ?max_tool_rounds:int
  -> Message.t Nonempty_list.t
  -> Response.t Deferred.Or_error.t
