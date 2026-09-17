open! Core

type t

(** PCRE syntax, matched against the whole hostname; a leading [!] inverts the match. *)
val of_string : string -> t

val of_string_or_error : string -> t Or_error.t
val to_string : t -> string
val matches : t -> hostname:string -> bool

include Sexpable.S with type t := t

(** [-hostname HOST], defaulting to this box's hostname. *)
val hostname_param : string Command.Param.t
