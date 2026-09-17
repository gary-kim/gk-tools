open! Core
open! Async
open Deferred.Or_error.Let_syntax

let load_entries ~packages =
  match packages with
  | Some path ->
    Sexp_macro.load_sexps (File_path.to_string path) [%of_sexp: Dnf_plan.Entry.t]
  | None ->
    let%bind config = Config.load_default () in
    Config.dnf config
    |> Or_error.of_option ~error:(Error.create_s [%message "config has no dnf section"])
    |> Or_error.map ~f:(fun (settings : Config.Dnf_settings.t) -> settings.packages)
    |> Deferred.return
;;

let compute_plan ~hostname ~packages ~offline =
  let%bind entries = load_entries ~packages
  and snapshot = Dnf_native.snapshot ~cacheonly:offline in
  Dnf_plan.plan snapshot ~entries ~hostname |> Deferred.return
;;

let print_plan plan = List.iter (Dnf_plan.to_lines plan) ~f:print_endline

let run_sudo_dnf ~offline ~args =
  let args = Bool.select offline ("--cacheonly" :: args) args in
  Logged_process.log ~prog:"sudo" ~args:("dnf" :: args);
  let%bind.Deferred () = Writer.flushed (force Writer.stdout) in
  let%bind pid =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Unix.fork_exec ~prog:"sudo" ~argv:([ "sudo"; "dnf" ] @ args) ())
  in
  Unix.waitpid pid
  |> Deferred.map ~f:Unix.Exit_or_signal.or_error
  |> Deferred.Or_error.tag_s ~tag:[%message "dnf failed" (args : string list)]
;;

let offer_autoremove ~yes ~offline =
  let%bind { Dnf_native.Unneeded.removable; protected } = Dnf_native.unneeded () in
  List.iter protected ~f:(fun name ->
    print_endline [%string "warning: unneeded but protected by dnf: %{name}"]);
  match removable with
  | [] -> return ()
  | _ :: _ as unneeded ->
    print_endline "autoremove:";
    List.iter unneeded ~f:(fun name -> print_endline ("  " ^ name));
    let%bind confirmed = Confirm.ask ~yes ~prompt:"Autoremove?" in
    if confirmed
    then run_sudo_dnf ~offline ~args:[ "autoremove" ]
    else (
      [%log.info "autoremove skipped"];
      return ())
;;

let apply ~hostname ~yes ~packages ~offline =
  let%bind plan = compute_plan ~hostname ~packages ~offline in
  print_plan plan;
  if not (Dnf_plan.has_actions plan)
  then offer_autoremove ~yes ~offline
  else (
    let%bind confirmed = Confirm.ask ~yes ~prompt:"Apply?" in
    if not confirmed
    then (
      [%log.info "apply skipped"];
      return ())
    else (
      let steps =
        List.filter
          ~f:(fun (_, packages) -> not (List.is_empty packages))
          [ [ "install" ], plan.install
          ; [ "mark"; "user" ], List.map plan.mark_user ~f:fst
          ; [ "mark"; "dependency" ], List.map plan.mark_dependency ~f:fst
          ]
      in
      let%bind () =
        Deferred.Or_error.List.iter steps ~how:`Sequential ~f:(fun (base, packages) ->
          run_sudo_dnf ~offline ~args:(base @ packages))
      in
      offer_autoremove ~yes ~offline))
;;

let dump ~offline =
  let%map snapshot = Dnf_native.snapshot ~cacheonly:offline in
  let entries, omitted = Dnf_plan.dump_entries snapshot in
  List.iter omitted ~f:(fun id ->
    print_endline ("; not in current comps, omitted: " ^ id));
  List.iter entries ~f:(fun entry ->
    print_endline (Sexp.to_string (Dnf_plan.Entry.sexp_of_t entry)))
;;

let packages_param =
  Command.Param.flag
    "packages"
    (Command.Param.optional File_path.arg_type)
    ~doc:"FILE package entries (as dump prints them) instead of the config's dnf section"
;;

let offline_param =
  Command.Param.flag
    "offline"
    Command.Param.no_arg
    ~doc:" use only cached repo metadata, never refreshing it"
;;

let plan_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Show the package reconciliation plan without changing anything"
    (let%map_open.Command hostname = Host_pattern.hostname_param
     and packages = packages_param
     and offline = offline_param
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%map plan = compute_plan ~hostname ~packages ~offline in
       print_plan plan)
;;

let apply_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Reconcile explicitly installed packages to match the config"
    (let%map_open.Command hostname = Host_pattern.hostname_param
     and yes = flag "yes" no_arg ~doc:" skip the confirmation prompt"
     and packages = packages_param
     and offline = offline_param
     and () = Log.Global.set_level_via_param () in
     fun () -> apply ~hostname ~yes ~packages ~offline)
;;

let dump_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Print config entries covering this box's current explicit installs"
    (let%map_open.Command offline = offline_param
     and () = Log.Global.set_level_via_param () in
     fun () -> dump ~offline)
;;

let readme () =
  String.strip
    {|
The dnf section of the config lists what should be explicitly installed
(dnf reasons User and Group). plan/apply reconcile the box against it:
missing packages are installed, wanted packages present as dependencies are
re-marked user, and explicit packages neither listed nor covered by a
declared group are demoted to dependencies. apply itself never removes
packages; when dnf reports unneeded packages after reconciling, it offers
to run dnf autoremove (its transaction prompt remains the final gate).

Entries are NAME or (NAME MODIFIER ...):

fzf
(tlp (Only_hostname_regex "laptop-.*"))
(cloud-server-environment group)

A group entry names a comps group or environment; it protects its member
packages from demotion. dump prints entries covering the current box as a
starting point for the config, and plan/apply read that same format from a
standalone file via -packages instead of the config's dnf section.

Repo metadata is refreshed as dnf itself would (per metadata_expire);
-offline reads only what is already cached.
|}
;;

let command =
  Command.group
    ~summary:"Reconcile dnf user-installed packages against the config"
    ~readme
    [ "plan", plan_command; "apply", apply_command; "dump", dump_command ]
;;
