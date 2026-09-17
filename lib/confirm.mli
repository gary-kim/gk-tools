open! Core
open! Async

(** A yes/no prompt on the tty, defaulting to no. [yes] skips it; without a tty on both
    stdin and stdout, asking is an error rather than a hang. *)
val ask : yes:bool -> prompt:string -> bool Deferred.Or_error.t
