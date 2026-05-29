open! Core
open! Async

module Caldav_upload_settings : sig
  type t =
    { server_url : string
    ; username : string option
    ; password : string option
    ; password_cmd : string option
    }
  [@@deriving sexp]
end

type t

val empty : t
val caldav_upload : t -> Caldav_upload_settings.t option
val default_path : unit -> File_path.t
val load : File_path.t -> t Deferred.Or_error.t
val load_default : unit -> t Deferred.Or_error.t
