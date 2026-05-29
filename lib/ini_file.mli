open! Core
open! Async

type t

val load : File_path.t -> t Deferred.Or_error.t
val get : t -> section:string -> key:string -> string option

(** Returns [(section, value)] of the first section (in file order) containing [key]. *)
val find_key : t -> key:string -> (string * string) option
