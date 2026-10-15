open! Core
open! Async

module Caldav_upload_settings : sig
  type t =
    { server_url : string
    ; username : string option
    ; password : Secret_source.t option
    }
  [@@deriving sexp, fields ~getters]
end

module Git_managed_sync_settings : sig
  module Repo : sig
    type t =
      { id : string
      ; dir : File_path.t
      }
    [@@deriving sexp]
  end

  type t = { repos : Repo.t list } [@@deriving sexp]
end

module Notmuch_git_settings : sig
  type t = { remote : string } [@@deriving sexp]
end

module Llm_completions_settings : sig
  module Reasoning_effort : sig
    type t =
      | None
      | Low
      | Medium
      | High
      | Xhigh
    [@@deriving sexp, string]
  end

  module Reasoning_summary : sig
    type t =
      | Auto
      | Concise
      | Detailed
    [@@deriving sexp, string]
  end

  module Url : sig
    type t [@@deriving sexp]

    val to_uri : t -> Uri.t
  end

  module Model : sig
    type t [@@deriving sexp, to_string]
  end

  module Max_output_tokens : sig
    type t [@@deriving sexp]

    val raw : t -> int
  end

  module Temperature : sig
    type t [@@deriving sexp]

    val raw : t -> float
  end

  module Timeout : sig
    type t [@@deriving sexp]

    val raw : t -> Time_ns.Span.t
  end

  type t =
    { url : Url.t
    ; token : Secret_source.t option
    ; model : Model.t
    ; system_prompt : String.Utf8.t option
    ; reasoning_effort : Reasoning_effort.t option
    ; reasoning_summary : Reasoning_summary.t option
    ; max_output_tokens : Max_output_tokens.t option
    ; temperature : Temperature.t option
    ; timeout : Timeout.t
    }
  [@@deriving sexp]
end

module Classify_mail_settings : sig
  module Destination : sig
    type t =
      { folder : File_path.Relative.t
      ; description : string
      }
    [@@deriving sexp]
  end

  type t =
    { destinations : Destination.t Nonempty_list.t
    ; system_prompt : String.Utf8.t option
    }
  [@@deriving sexp]
end

type t

val empty : t
val caldav_upload : t -> Caldav_upload_settings.t option
val git_managed_sync : t -> Git_managed_sync_settings.t option
val notmuch_git : t -> Notmuch_git_settings.t option
val llm_completions : t -> Llm_completions_settings.t option
val classify_mail : t -> Classify_mail_settings.t option
val default_path : unit -> File_path.t
val load : File_path.t -> t Deferred.Or_error.t
val load_default : unit -> t Deferred.Or_error.t
