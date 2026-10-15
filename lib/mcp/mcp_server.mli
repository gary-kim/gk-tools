open! Core
open! Async

module Context : sig
  type t

  val create : unit -> t
  val cancelled : t -> unit Deferred.t
  val is_cancelled : t -> bool
  val cancel : t -> unit
end

module Implementation : sig
  type 'state t

  val lift : 'a t -> f:('b -> 'a) -> 'b t
end

module Implementations : sig
  type 'state t

  val create : implementations:'state Implementation.t list -> 'state t Or_error.t
  val lift : 'a t -> f:('b -> 'a) -> 'b t
end

type 'state t

val create
  :  info:Mcp_protocol.Implementation.t
  -> ?instructions:string
  -> implementations:'state Implementations.t
  -> state:'state
  -> unit
  -> 'state t

module Tool : sig
  type 'state server := 'state t
  type ('query, 'response) t

  val create
    :  name:string
    -> ?description:string
    -> input_schema:Mcp_protocol.Schema.Object.t
    -> ?output_schema:Mcp_protocol.Schema.Object.t
    -> query_of_jsonaf:(Jsonaf.t -> 'query)
    -> response_to_result:('response -> Mcp_protocol.Tool.Result.t)
    -> unit
    -> ('query, 'response) t

  val definition : (_, _) t -> Mcp_protocol.Tool.t

  val implement
    :  ('query, 'response) t
    -> ('state -> Context.t -> 'query -> 'response Deferred.Or_error.t)
    -> 'state Implementation.t

  val dispatch
    :  ('query, 'response) t
    -> 'state server
    -> ?context:Context.t
    -> 'query
    -> 'response Deferred.Or_error.t
end

module Resource : sig
  type 'state server := 'state t
  type t

  val create
    :  uri:string
    -> name:string
    -> ?description:string
    -> ?mime_type:Mcp_protocol.Mime_type.t
    -> unit
    -> t

  val definition : t -> Mcp_protocol.Resource.t

  val implement
    :  t
    -> ('state -> Context.t -> Mcp_protocol.Resource.Contents.t list Deferred.Or_error.t)
    -> 'state Implementation.t

  val dispatch
    :  t
    -> 'state server
    -> ?context:Context.t
    -> unit
    -> Mcp_protocol.Resource.Contents.t list Deferred.Or_error.t
end

module Prompt : sig
  type 'state server := 'state t
  type t

  val create
    :  name:string
    -> ?description:string
    -> ?arguments:Mcp_protocol.Prompt.Argument.t list
    -> unit
    -> t Or_error.t

  val definition : t -> Mcp_protocol.Prompt.t

  val implement
    :  t
    -> ('state
        -> Context.t
        -> string String.Map.t
        -> Mcp_protocol.Prompt.Result.t Deferred.Or_error.t)
    -> 'state Implementation.t

  val dispatch
    :  t
    -> 'state server
    -> ?context:Context.t
    -> string String.Map.t
    -> Mcp_protocol.Prompt.Result.t Deferred.Or_error.t
end

val list_tools : _ t -> Mcp_protocol.Tool.t list
val list_resources : _ t -> Mcp_protocol.Resource.t list
val list_prompts : _ t -> Mcp_protocol.Prompt.t list

val call_tool
  :  _ t
  -> ?context:Context.t
  -> name:string
  -> arguments:Jsonaf.t Mcp_protocol.Json_object.t
  -> unit
  -> (Mcp_protocol.Tool.Result.t, Mcp_protocol.Rpc_error.t) Deferred.Result.t

val dispatch
  :  _ t
  -> context:Context.t
  -> method_:string
  -> params:Jsonaf.t Mcp_protocol.Json_object.t option
  -> (Jsonaf.t, Mcp_protocol.Rpc_error.t) Deferred.Result.t

module Session : sig
  type 'state server := 'state t
  type 'state t

  val create : 'state server -> 'state t
  val close : _ t -> unit

  val notify
    :  _ t
    -> method_:string
    -> params:Jsonaf.t Mcp_protocol.Json_object.t option
    -> unit

  val request
    :  _ t
    -> id:Mcp_protocol.Request.Id.t
    -> method_:string
    -> params:Jsonaf.t Mcp_protocol.Json_object.t option
    -> (Jsonaf.t, Mcp_protocol.Rpc_error.t) Result.t option Deferred.t
end
