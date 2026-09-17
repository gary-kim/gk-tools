open! Core
open! Async

module Raw = struct
  (* Field order matches snapshot_value in dnf_native_stubs.cpp. *)
  type t =
    { packages : (string * int) list
    ; groups : (string * string list) list
    ; environments : (string * string list) list
    ; installed_groups : string list
    ; installed_environments : string list
    }
end

module Unneeded = struct
  type t =
    { removable : string list
    ; protected : string list
    }
  [@@deriving sexp]
end

external raw_snapshot : cacheonly:bool -> (Raw.t, string) Result.t = "gkt_dnf5_snapshot"

external raw_unneeded
  :  unit
  -> (string list * string list, string) Result.t
  = "gkt_dnf5_unneeded"

let to_snapshot (raw : Raw.t) : Dnf_plan.Snapshot.t Or_error.t =
  let open Or_error.Let_syntax in
  let%map packages =
    List.map raw.packages ~f:(fun (name, tag) ->
      Dnf_plan.Reason.of_tag tag |> Or_error.map ~f:(fun reason -> name, reason))
    |> Or_error.combine_errors
    |> Or_error.map ~f:Dnf_plan.installed_of_alist
  in
  let member_sets pairs =
    List.map pairs ~f:(fun (id, members) -> id, Set.of_list (module String) members)
    |> Map.of_alist_reduce (module String) ~f:Set.union
  in
  { Dnf_plan.Snapshot.packages
  ; groups = member_sets raw.groups
  ; environments = member_sets raw.environments
  ; installed_groups = Set.of_list (module String) raw.installed_groups
  ; installed_environments = Set.of_list (module String) raw.installed_environments
  }
;;

let load_override path of_sexp = Reader.load_sexp path of_sexp

let snapshot ~cacheonly =
  match Sys.getenv "GKT_DNF_SNAPSHOT" with
  | Some path -> load_override path [%of_sexp: Dnf_plan.Snapshot.t]
  | None ->
    (match%bind.Deferred In_thread.run (fun () -> raw_snapshot ~cacheonly) with
     | Error message -> Deferred.Or_error.error_string message
     | Ok raw -> to_snapshot raw |> Deferred.return)
;;

let unneeded () =
  let result =
    match Sys.getenv "GKT_DNF_UNNEEDED" with
    | Some path -> load_override path [%of_sexp: Unneeded.t]
    | None ->
      (match%map.Deferred In_thread.run (fun () -> raw_unneeded ()) with
       | Error message -> Or_error.error_string message
       | Ok (removable, protected) -> Ok { Unneeded.removable; protected })
  in
  Deferred.Or_error.map result ~f:(fun { Unneeded.removable; protected } ->
    { Unneeded.removable = List.dedup_and_sort removable ~compare:String.compare
    ; protected = List.dedup_and_sort protected ~compare:String.compare
    })
;;
