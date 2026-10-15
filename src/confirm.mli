open! Core
open! Async

val ask : yes:bool -> prompt:string -> bool Deferred.Or_error.t
val yes_param : bool Command.Param.t
