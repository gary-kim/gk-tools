open! Core
open! Async

module Field : sig
  type t =
    { name : string option
    ; value : string option
    }
  [@@deriving jsonaf, sexp_of]
end

module Record : sig
  type t =
    { name : string
    ; fields : Field.t list
    ; notes : string option
    }
  [@@deriving jsonaf, sexp_of]

  val field : t -> string -> string option
end

(** Runs [rbw sync] with a timeout. *)
val sync : unit -> unit Deferred.Or_error.t

(** Returns whether the vault is unlocked without prompting. *)
val is_unlocked : unit -> bool Deferred.t

(** Returns matching record names with the ["<folder>/"] prefix stripped. *)
val search : folder:string -> term:string -> string list Deferred.Or_error.t

val get : folder:string -> name:string -> Record.t Deferred.Or_error.t

(** Strips [rbw]'s trailing newline. *)
val get_field : folder:string -> name:string -> field:string -> string Deferred.Or_error.t

val edit_with_content
  :  folder:string
  -> name:string
  -> contents:string
  -> unit Deferred.Or_error.t
