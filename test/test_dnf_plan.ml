open! Core
open! Async
module Dnf_plan = Gk_tools.Dnf_plan

let%expect_test "Entry - config syntax roundtrip" =
  let test str =
    print_s [%sexp ([%of_sexp: Dnf_plan.Entry.t] (Sexp.of_string str) : Dnf_plan.Entry.t)]
  in
  test "git";
  [%expect {| git |}];
  test "(fonts group)";
  [%expect {| (fonts Group) |}];
  test {|(tlp (Only_hostname_regex "laptop-.*"))|};
  [%expect {| (tlp (Only_hostname_regex laptop-.*)) |}];
  test {|(cloud-server-environment group (Only_hostname_regex "cloud-.*"))|};
  [%expect {| (cloud-server-environment Group (Only_hostname_regex cloud-.*)) |}];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> [%of_sexp: Dnf_plan.Entry.t] (Sexp.of_string "-y"))
       : Dnf_plan.Entry.t Or_error.t)];
  [%expect
    {| (Error (Of_sexp_error "entry name may not start with [-]" (invalid_sexp -y))) |}];
  return ()
;;

let%expect_test "Reason - of_tag matches the stub's tags" =
  List.iter [ 0; 1; 2; 3; 4; 5; 6; 7; -1 ] ~f:(fun tag ->
    print_s
      [%sexp (tag : int), (Dnf_plan.Reason.of_tag tag : Dnf_plan.Reason.t Or_error.t)]);
  [%expect
    {|
    (0 (Ok Unknown))
    (1 (Ok Dependency))
    (2 (Ok User))
    (3 (Ok Clean))
    (4 (Ok Weak_dependency))
    (5 (Ok Group))
    (6 (Ok External_user))
    (7 (Error ("unknown reason tag" (tag 7))))
    (-1 (Error ("unknown reason tag" (tag -1))))
    |}];
  return ()
;;

let snapshot str = [%of_sexp: Dnf_plan.Snapshot.t] (Sexp.of_string str)

let box_snapshot =
  snapshot
    {|((packages ((bash Group) (default-fonts-a Group) (git User) (htop Dependency)
                 (libweak Weak_dependency) (stray User) (zlib Dependency)))
       (groups ((core (bash filesystem)) (extra (extrapkg)) (fonts (default-fonts-a))))
       (environments ((server-env (core))))
       (installed_groups (core fonts))
       (installed_environments (server-env)))|}
;;

let entries strs =
  List.map strs ~f:(fun str -> [%of_sexp: Dnf_plan.Entry.t] (Sexp.of_string str))
;;

let test_plan snapshot ~entries:strs ~hostname =
  print_s
    [%sexp
      (Dnf_plan.plan snapshot ~entries:(entries strs) ~hostname : Dnf_plan.t Or_error.t)]
;;

let box_entries =
  [ "git"
  ; "htop"
  ; "libweak"
  ; "fzf"
  ; {|(tlp (Only_hostname_regex "laptop-.*"))|}
  ; "(server-env group)"
  ]
;;

let%expect_test "plan - reconcile with environment protection" =
  test_plan box_snapshot ~entries:box_entries ~hostname:"server-1";
  [%expect
    {|
    (Ok
     ((install (fzf)) (mark_user ((htop Dependency) (libweak Weak_dependency)))
      (mark_dependency ((default-fonts-a Group) (stray User)))
      (warnings ((Installed_group_not_declared (id fonts))))))
    |}];
  return ()
;;

let%expect_test "plan - host-scoped entry joins on a matching hostname" =
  test_plan box_snapshot ~entries:box_entries ~hostname:"laptop-1";
  [%expect
    {|
    (Ok
     ((install (fzf tlp))
      (mark_user ((htop Dependency) (libweak Weak_dependency)))
      (mark_dependency ((default-fonts-a Group) (stray User)))
      (warnings ((Installed_group_not_declared (id fonts))))))
    |}];
  return ()
;;

let%expect_test "plan - declared group not installed still protects, and warns" =
  test_plan
    (snapshot
       {|((packages ((extrapkg User) (git User)))
          (groups ((extra (extrapkg))))
          (environments ())
          (installed_groups ())
          (installed_environments ()))|})
    ~entries:[ "git"; "(extra group)" ]
    ~hostname:"server-1";
  [%expect
    {|
    (Ok
     ((install ()) (mark_user ()) (mark_dependency ())
      (warnings ((Group_not_installed (name extra))))))
    |}];
  return ()
;;

let%expect_test "plan - environment member group missing from comps errors" =
  let snapshot =
    snapshot
      {|((packages ((git User)))
         (groups ())
         (environments ((server-env (core))))
         (installed_groups ())
         (installed_environments (server-env)))|}
  in
  test_plan snapshot ~entries:[ "(server-env group)" ] ~hostname:"h";
  [%expect
    {|
    (Error
     ("environment references groups missing from current comps"
      (name server-env) (missing (core))))
    |}];
  return ()
;;

let%expect_test "plan - unknown group errors" =
  test_plan box_snapshot ~entries:[ "git"; "(nosuch group)" ] ~hostname:"server-1";
  [%expect {| (Error ("unknown group or environment" (name nosuch))) |}];
  return ()
;;

let%expect_test "plan - clean box has no actions" =
  test_plan
    box_snapshot
    ~entries:[ "git"; "stray"; "(server-env group)"; "(fonts group)" ]
    ~hostname:"server-1";
  [%expect {| (Ok ((install ()) (mark_user ()) (mark_dependency ()) (warnings ()))) |}];
  return ()
;;

let%expect_test "dump_entries" =
  let print_dump snapshot =
    let entries, omitted = Dnf_plan.dump_entries snapshot in
    print_s [%message (omitted : string list)];
    List.iter entries ~f:(fun entry ->
      print_endline (Sexp.to_string (Dnf_plan.Entry.sexp_of_t entry)))
  in
  print_dump box_snapshot;
  [%expect
    {|
    (omitted ())
    (server-env Group)
    (fonts Group)
    git
    stray
    |}];
  print_dump
    (snapshot
       {|((packages ((git User)))
          (groups ((core (bash))))
          (environments ((server-env (gone-group))))
          (installed_groups (core oldgroup))
          (installed_environments (server-env)))|});
  [%expect {|
    (omitted (server-env oldgroup))
    (core Group)
    git
    |}];
  return ()
;;
