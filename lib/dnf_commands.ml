open! Core
open! Async
open Deferred.Or_error.Let_syntax
module Package_name = Dnf_plan.Package_name
module Group_id = Dnf_plan.Group_id

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
  (let%bind status = Logged_process.spawn ~prog:"sudo" ~args:("dnf" :: args) () in
   Unix.Exit_or_signal.or_error status |> Deferred.return)
  |> Deferred.Or_error.tag_s ~tag:[%message "dnf failed" (args : string list)]
;;

let dnf_subcommand (action : Dnf_plan.Step.Action.t) =
  match action with
  | Install -> [ "install" ]
  | Mark_user -> [ "--setopt=disable_excludes=*"; "mark"; "user" ]
  | Mark_dependency -> [ "--setopt=disable_excludes=*"; "mark"; "dependency" ]
;;

let offer_autoremove ~yes ~offline =
  let%bind { Dnf_native.Unneeded.removable; protected } = Dnf_native.unneeded () in
  List.iter protected ~f:(fun name ->
    print_endline [%string "warning: unneeded but protected by dnf: %{name#Package_name}"]);
  match removable with
  | [] -> return ()
  | _ :: _ as unneeded ->
    Dnf_plan.section_lines
      ~title:"autoremove:"
      (List.map unneeded ~f:Package_name.to_string)
    |> List.iter ~f:print_endline;
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
  match Dnf_plan.steps plan with
  | [] -> offer_autoremove ~yes ~offline
  | _ :: _ as steps ->
    let%bind confirmed = Confirm.ask ~yes ~prompt:"Apply?" in
    if not confirmed
    then (
      [%log.info "apply skipped"];
      return ())
    else (
      let%bind () =
        Deferred.Or_error.List.iter
          steps
          ~how:`Sequential
          ~f:(fun { Dnf_plan.Step.action; packages } ->
            let packages =
              Nonempty_list.map packages ~f:Package_name.to_string
              |> Nonempty_list.to_list
            in
            run_sudo_dnf ~offline ~args:(dnf_subcommand action @ packages))
      in
      offer_autoremove ~yes ~offline)
;;

let dump ~offline =
  let%map snapshot = Dnf_native.snapshot ~cacheonly:offline in
  let entries, omitted = Dnf_plan.dump_entries snapshot in
  List.iter omitted ~f:(fun id ->
    print_endline [%string "; not in current comps, omitted: %{id#Group_id}"]);
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
     and yes = Confirm.yes_param
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
(dnf reasons User, Group, and External User). plan/apply reconcile the box
against it: missing packages are installed, wanted packages present as
dependencies are re-marked user, and explicit packages neither listed nor
covered by a declared group are demoted to dependencies. apply itself never
removes packages; when dnf reports unneeded packages after reconciling, it
offers to run dnf autoremove (its transaction prompt remains the final gate).

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
