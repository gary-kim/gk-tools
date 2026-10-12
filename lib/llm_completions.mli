open! Core
open! Async

module Message : sig
  module Role : sig
    type t =
      | User
      | Assistant
      | System
      | Developer
    [@@deriving sexp]
  end

  type t =
    { role : Role.t
    ; content : String.Utf8.t
    }
end

module Response : sig
  type t =
    { content : String.Utf8.t
    ; reasoning : String.Utf8.t list
    }
  [@@deriving sexp_of]
end

type t

val create : Config.Llm_completions_settings.t -> t Deferred.Or_error.t

val complete
  :  t
  -> ?instructions:String.Utf8.t
  -> Message.t Nonempty_list.t
  -> Response.t Deferred.Or_error.t
