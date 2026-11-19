open! Core

val command : Command.t

module For_testing : sig
  val encode_request : write:bool -> string
  val verify_readback : string -> unit Or_error.t
end
