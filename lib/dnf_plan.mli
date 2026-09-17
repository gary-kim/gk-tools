open! Core

module Reason : sig
  type t =
    | User
    | Group
    | External_user
    | Dependency
    | Weak_dependency
    | Clean
    | Unknown
  [@@deriving sexp]

  (** Tag values are produced by reason_tag in dnf_native_stubs.cpp. *)
  val of_tag : int -> t Or_error.t

  (** [User], [Group], and [External_user] make up the reconciled universe; everything
      else is left alone. *)
  val is_explicit : t -> bool
end

module Modifier : sig
  type t =
    | Group
    | Only_hostname_regex of Host_pattern.t
  [@@deriving sexp]
end

module Entry : sig
  (** A config entry: a bare [NAME] atom or [(NAME MODIFIER ...)]. *)
  type t =
    { name : string
    ; modifiers : Modifier.t list
    }
  [@@deriving sexp]

  val is_group : t -> bool
  val applies : t -> hostname:string -> bool
end

module Snapshot : sig
  (** [groups] and [environments] hold definitions from current repo metadata only, never
      the installed-state snapshot, so membership does not derive from what was chosen at
      installation time. Group members are the mandatory, default, and conditional
      packages (what installing the group installs); an environment expands to its
      required groups. *)
  type t =
    { packages : Reason.t Map.M(String).t
    ; groups : Set.M(String).t Map.M(String).t
    ; environments : Set.M(String).t Map.M(String).t
    ; installed_groups : Set.M(String).t
    ; installed_environments : Set.M(String).t
    }
  [@@deriving sexp]
end

module Warning : sig
  type t =
    | Group_not_installed of { name : string }
    | Installed_group_not_declared of { id : string }
  [@@deriving sexp_of]
end

type t =
  { install : string list
  ; mark_user : (string * Reason.t) list
  ; mark_dependency : (string * Reason.t) list
  ; warnings : Warning.t list
  }
[@@deriving sexp_of]

val installed_of_alist : (string * Reason.t) list -> Reason.t Map.M(String).t
val plan : Snapshot.t -> entries:Entry.t list -> hostname:string -> t Or_error.t

(** Also returns the installed group/environment ids that [plan] could not resolve against
    current comps, which are omitted. *)
val dump_entries : Snapshot.t -> Entry.t list * string list

val has_actions : t -> bool
val to_lines : t -> string list
