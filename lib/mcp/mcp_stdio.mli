open! Core
open! Async

val serve
  :  _ Mcp_server.t
  -> reader:Reader.t
  -> writer:Writer.t
  -> unit Deferred.Or_error.t

val run : _ Mcp_server.t -> unit Deferred.Or_error.t
