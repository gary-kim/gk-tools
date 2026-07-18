open! Core
open! Async

val log : prog:string -> args:string list -> unit

val run
  :  ?stdin:string
  -> prog:string
  -> args:string list
  -> unit
  -> string Deferred.Or_error.t

val create : prog:string -> args:string list -> unit -> Process.t Deferred.Or_error.t
