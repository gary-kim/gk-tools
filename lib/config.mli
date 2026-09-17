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

module Dnf_settings : sig
  type t = { packages : Dnf_plan.Entry.t list } [@@deriving sexp]
end

module Git_managed_sync_settings : sig
  module Repo : sig
    type t =
      { id : string
      ; dir : string
      }
    [@@deriving sexp]
  end

  type t = { repos : Repo.t list } [@@deriving sexp]
end

module Notmuch_git_settings : sig
  type t = { remote : string } [@@deriving sexp]
end

type t

val empty : t
val caldav_upload : t -> Caldav_upload_settings.t option
val dnf : t -> Dnf_settings.t option
val git_managed_sync : t -> Git_managed_sync_settings.t option
val notmuch_git : t -> Notmuch_git_settings.t option
val default_path : unit -> File_path.t
val load : File_path.t -> t Deferred.Or_error.t
val load_default : unit -> t Deferred.Or_error.t
