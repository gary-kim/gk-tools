open! Core

type t

val parse : string -> t Or_error.t
val uid : t -> string
val cleaned : t -> string
