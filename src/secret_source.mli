open! Core
open! Async

type t =
  | Literal of string
  | Env of string
  | Command of string
[@@deriving sexp]

(** Accepts [(Literal VALUE)], [(Env NAME)], or [(Command SHELL-COMMAND)]. *)
val arg_type : t Command.Arg_type.t

(** With [prefix], [-literal], [-env], and [-cmd] become [-PREFIX], [-PREFIX-env], and
    [-PREFIX-cmd]. *)
val param
  :  ?prefix:string
  -> doc:string
  -> if_nothing_chosen:(t, 'a) Command.Param.If_nothing_chosen.t
  -> unit
  -> 'a Command.Param.t

(** Commands use the controlling terminal for stdin/stderr and capture stdout. *)
val resolve : t -> string Deferred.Or_error.t
