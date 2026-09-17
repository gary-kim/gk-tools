open! Core
module Package_name : Identifiable.S
module Group_id : Identifiable.S

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
end

module Entry : sig
  module Target : sig
    type t =
      | Package of Package_name.t
      | Group of Group_id.t
  end

  (** A config entry: a bare [NAME] atom or [(NAME MODIFIER ...)]. *)
  type t =
    { target : Target.t
    ; host_patterns : Host_pattern.t list
    }
  [@@deriving sexp]
end

module Snapshot : sig
  (** [groups] and [environments] hold definitions from current repo metadata only, never
      the installed-state snapshot, so membership does not derive from what was chosen at
      installation time. Group members are the mandatory, default, and conditional
      packages (what installing the group installs); an environment expands to its
      required and default groups. *)
  type t =
    { packages : Reason.t Map.M(Package_name).t
    ; groups : Set.M(Package_name).t Map.M(Group_id).t
    ; environments : Set.M(Group_id).t Map.M(Group_id).t
    ; installed_groups : Set.M(Group_id).t
    ; installed_environments : Set.M(Group_id).t
    }
  [@@deriving sexp]
end

module Step : sig
  module Action : sig
    type t =
      | Install
      | Mark_user
      | Mark_dependency
  end

  type t =
    { action : Action.t
    ; packages : Package_name.t Nonempty_list.t
    }
end

type t [@@deriving sexp_of]

val installed_of_alist
  :  (Package_name.t * Reason.t) list
  -> Reason.t Map.M(Package_name).t

val plan : Snapshot.t -> entries:Entry.t list -> hostname:string -> t Or_error.t

(** Also returns the installed group/environment ids that [plan] could not resolve against
    current comps, which are omitted. *)
val dump_entries : Snapshot.t -> Entry.t list * Group_id.t list

val steps : t -> Step.t list
val section_lines : title:string -> string list -> string list
val to_lines : t -> string list
