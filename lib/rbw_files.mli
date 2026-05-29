open! Core

val command : Command.t

module For_testing : sig
  module Resolved : sig
    type t =
      { name : string
      ; local_target : string
      ; remote_content : string
      ; mode : int option
      }
    [@@deriving sexp_of]
  end

  val resolve_target : home_dir:string -> string -> string

  val resolve
    :  ?strict:bool
    -> home_dir:string
    -> Rbw_cli.Record.t
    -> Resolved.t Or_error.t

  val apply_record : Resolved.t -> unit Async.Deferred.Or_error.t
end
