open! Core
open! Async

val run
  :  ?stdin:string
  -> prog:string
  -> args:string list
  -> unit
  -> string Deferred.Or_error.t

val create : prog:string -> args:string list -> unit -> Process.t Deferred.Or_error.t

val spawn
  :  ?env:[ Unix.env | `Replace_raw of string list ]
  -> prog:string
  -> args:string list
  -> unit
  -> Unix.Exit_or_signal.t Deferred.Or_error.t
