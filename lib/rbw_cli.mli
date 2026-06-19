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

(** [sync ()] runs [rbw sync] with a timeout. *)
val sync : unit -> unit Deferred.Or_error.t

(** [is_unlocked ()] runs [rbw unlocked] and returns whether the vault is unlocked. Does
    NOT trigger an unlock prompt. *)
val is_unlocked : unit -> bool Deferred.t

(** [search ~folder ~term] runs [rbw search --folder FOLDER TERM] and returns the matching
    record names with the leading "FOLDER/" prefix stripped. *)
val search : folder:string -> term:string -> string list Deferred.Or_error.t

(** [get ~folder ~name] runs [rbw get --raw --folder FOLDER NAME] and parses the JSON. *)
val get : folder:string -> name:string -> Record.t Deferred.Or_error.t

(** [get_field ~folder ~name ~field] runs [rbw get --field FIELD --folder FOLDER NAME] and
    returns the field value with rbw's trailing newline removed. *)
val get_field : folder:string -> name:string -> field:string -> string Deferred.Or_error.t

val edit_with_content
  :  folder:string
  -> name:string
  -> contents:string
  -> unit Deferred.Or_error.t
