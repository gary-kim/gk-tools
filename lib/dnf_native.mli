open! Core
open! Async

(** Reads via libdnf5 rather than dnf's output. [GKT_DNF_SNAPSHOT] and [GKT_DNF_UNNEEDED]
    override with sexp files for tests. *)

module Unneeded : sig
  (** [protected]: unneeded, but dnf's protected_packages config blocks removal. *)
  type t =
    { removable : string list
    ; protected : string list
    }
  [@@deriving sexp]
end

val snapshot : cacheonly:bool -> Dnf_plan.Snapshot.t Deferred.Or_error.t
val unneeded : unit -> Unneeded.t Deferred.Or_error.t
