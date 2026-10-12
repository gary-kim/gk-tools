open! Core
open! Async

type t =
  | Literal of string
  | Env of string
  | Command of string
[@@deriving sexp]

(** Commands use the controlling terminal for stdin/stderr and capture stdout. *)
val resolve : t -> string Deferred.Or_error.t
