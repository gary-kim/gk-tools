open! Core
open! Async
module Package_name = Dnf_plan.Package_name
module Group_id = Dnf_plan.Group_id

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
    { removable : Package_name.t list
    ; protected : Package_name.t list
    }
  [@@deriving sexp]
end

external raw_snapshot : cacheonly:bool -> (Raw.t, string) Result.t = "gkt_dnf5_snapshot"

external raw_unneeded
  :  unit
  -> (string list * string list, string) Result.t
  = "gkt_dnf5_unneeded"

external raw_reason_tags : unit -> (string * int) list = "gkt_dnf5_reason_tags"

let reason_of_tag tag : Dnf_plan.Reason.t Or_error.t =
  match tag with
  | 0 -> Ok Unknown
  | 1 -> Ok Dependency
  | 2 -> Ok User
  | 3 -> Ok Clean
  | 4 -> Ok Weak_dependency
  | 5 -> Ok Group
  | 6 -> Ok External_user
  | _ -> Or_error.error_s [%message "unknown reason tag" (tag : int)]
;;

let to_snapshot (raw : Raw.t) : Dnf_plan.Snapshot.t Or_error.t =
  let%map.Or_error packages =
    List.map raw.packages ~f:(fun (name, tag) ->
      reason_of_tag tag
      |> Or_error.map ~f:(fun reason -> Package_name.of_string name, reason))
    |> Or_error.combine_errors
    |> Or_error.map ~f:Dnf_plan.installed_of_alist
  in
  let group_ids ids =
    List.map ids ~f:Group_id.of_string |> Set.of_list (module Group_id)
  in
  let by_group_id pairs ~members =
    List.map pairs ~f:(fun (id, names) -> Group_id.of_string id, members names)
    |> Map.of_alist_reduce (module Group_id) ~f:Set.union
  in
  { Dnf_plan.Snapshot.packages
  ; groups =
      by_group_id raw.groups ~members:(fun names ->
        List.map names ~f:Package_name.of_string |> Set.of_list (module Package_name))
  ; environments = by_group_id raw.environments ~members:group_ids
  ; installed_groups = group_ids raw.installed_groups
  ; installed_environments = group_ids raw.installed_environments
  }
;;

let snapshot ~cacheonly =
  match Sys.getenv "GKT_DNF_SNAPSHOT" with
  | Some path -> Reader.load_sexp path [%of_sexp: Dnf_plan.Snapshot.t]
  | None ->
    (match%bind.Deferred In_thread.run (fun () -> raw_snapshot ~cacheonly) with
     | Error message -> Deferred.Or_error.error_string message
     | Ok raw -> to_snapshot raw |> Deferred.return)
;;

let unneeded () =
  let%map.Deferred.Or_error { Unneeded.removable; protected } =
    match Sys.getenv "GKT_DNF_UNNEEDED" with
    | Some path -> Reader.load_sexp path [%of_sexp: Unneeded.t]
    | None ->
      (match%map.Deferred In_thread.run (fun () -> raw_unneeded ()) with
       | Error message -> Or_error.error_string message
       | Ok (removable, protected) ->
         Or_error.try_with (fun () ->
           { Unneeded.removable = List.map removable ~f:Package_name.of_string
           ; protected = List.map protected ~f:Package_name.of_string
           }))
  in
  { Unneeded.removable = List.dedup_and_sort removable ~compare:Package_name.compare
  ; protected = List.dedup_and_sort protected ~compare:Package_name.compare
  }
;;

module For_testing = struct
  let reason_tags () =
    List.map (raw_reason_tags ()) ~f:(fun (name, tag) -> name, reason_of_tag tag)
  ;;
end
