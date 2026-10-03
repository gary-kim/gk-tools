open! Core
open! Async
open Deferred.Or_error.Let_syntax
module Repo = Config.Git_managed_sync_settings.Repo

let config_overrides = [ "-c"; "commit.gpgsign=false" ]

let git ~dir args =
  Logged_process.run
    ~prog:"git"
    ~args:(config_overrides @ ("-C" :: File_path.to_string dir :: args))
    ()
;;

let has_changes ~dir =
  let%map output = git ~dir [ "status"; "--porcelain"; "--untracked-files=normal" ] in
  not (String.is_empty (String.strip output))
;;

let conflicted_paths ~dir =
  let%map output = git ~dir [ "diff"; "--name-only"; "--diff-filter=U" ] in
  String.split_lines output
;;

let rebase_in_progress ~dir =
  let%bind output = git ~dir [ "rev-parse"; "--absolute-git-dir" ] in
  let git_dir = String.strip output in
  let state_dir_exists state_dir =
    match%map.Deferred Sys.file_exists (git_dir ^/ state_dir) with
    | `Yes -> true
    | `No | `Unknown -> false
  in
  let%map.Deferred rebase_merge = state_dir_exists "rebase-merge"
  and rebase_apply = state_dir_exists "rebase-apply" in
  Ok (rebase_merge || rebase_apply)
;;

let commit_all ~dir ~message =
  let%bind (_ : string) = git ~dir [ "add"; "--all" ] in
  let%map (_ : string) = git ~dir [ "commit"; "--message"; message ] in
  [%log.info "committed" (dir : File_path.t) (message : string)]
;;

let pull_rebase ~dir = git ~dir [ "pull"; "--rebase" ] |> Deferred.Or_error.ignore_m

let outgoing_commit_count ~dir =
  let%bind output = git ~dir [ "rev-list"; "--count"; "@{upstream}..HEAD" ] in
  let count = String.strip output in
  Int.of_string_opt count
  |> Or_error.of_option_lazy_sexp
       ~error:[%lazy_message "unexpected rev-list output" (count : string)]
  |> Deferred.return
;;

let print_outgoing_diff ~dir =
  let%bind.Deferred is_tty = Unix.isatty (Fd.stdout ()) in
  let color = if is_tty then "always" else "never" in
  let%map diff =
    git ~dir [ "--no-pager"; "diff"; [%string "--color=%{color}"]; "@{upstream}..HEAD" ]
  in
  print_string diff
;;

let sync ~dir ~yes ~message =
  [%log.info "syncing" (dir : File_path.t)];
  let%bind () =
    match%bind conflicted_paths ~dir with
    | [] -> return ()
    | _ :: _ as paths ->
      Deferred.Or_error.error_s
        [%message
          "refusing to sync: unresolved merge conflicts"
            (dir : File_path.t)
            (paths : string list)]
  in
  let%bind () =
    match%bind rebase_in_progress ~dir with
    | false -> return ()
    | true ->
      Deferred.Or_error.error_s
        [%message "refusing to sync: rebase in progress" (dir : File_path.t)]
  in
  let%bind () =
    match%bind has_changes ~dir with
    | true -> commit_all ~dir ~message
    | false ->
      [%log.info "nothing to commit" (dir : File_path.t)];
      return ()
  in
  let%bind () = pull_rebase ~dir in
  match%bind outgoing_commit_count ~dir with
  | 0 ->
    [%log.info "nothing to push" (dir : File_path.t)];
    return ()
  | count ->
    let%bind () = print_outgoing_diff ~dir in
    let%bind confirmed =
      if yes
      then return true
      else (
        match%bind.Deferred
          Deferred.both (Unix.isatty (Fd.stdin ())) (Unix.isatty (Fd.stdout ()))
        with
        | true, true ->
          Deferred.Or_error.try_with ~extract_exn:true (fun () ->
            Async_interactive.ask_yn
              ~default:false
              [%string "Push %{count#Int} commit(s) to %{dir#File_path}?"])
        | _ ->
          Deferred.Or_error.error_s
            [%message
              "stdin/stdout is not a tty: pass -yes to skip the confirmation prompt"])
    in
    if confirmed
    then (
      let%map (_ : string) = git ~dir [ "push" ] in
      [%log.info "pushed" (dir : File_path.t) (count : int)])
    else (
      [%log.info "push skipped" (dir : File_path.t)];
      return ())
;;

let sync_all ~dirs ~yes ~message =
  Nonempty_list.to_list dirs
  |> Deferred.List.map ~how:`Sequential ~f:(fun dir ->
    sync ~dir ~yes ~message
    |> Deferred.Or_error.tag_s ~tag:[%message "sync failed" (dir : File_path.t)])
  |> Deferred.map ~f:Or_error.combine_errors_unit
;;

let expand_home path =
  match Option.bind (File_path.to_relative path) ~f:Home_path.chop_tilde with
  | None -> Ok path
  | Some rest ->
    Home_path.from_env ()
    |> Or_error.tag_s ~tag:[%message "cannot expand path" (path : File_path.t)]
    |> Or_error.map ~f:(fun home ->
      File_path.of_absolute (Home_path.under_home ~home ~rest))
;;

let expand_home_all paths =
  Nonempty_list.map paths ~f:expand_home |> Nonempty_list.combine_or_errors
;;

let resolve_ids ~repos ids =
  Nonempty_list.map ids ~f:(fun id ->
    List.find_map repos ~f:(fun (repo : Repo.t) ->
      Option.some_if (String.equal repo.id id) repo.dir)
    |> Or_error.of_option_lazy_sexp ~error:[%lazy_message "unknown repo id" (id : string)])
  |> Nonempty_list.combine_or_errors
;;

let configured_repos () =
  let%bind config = Config.load_default () in
  let repos =
    Config.git_managed_sync config
    |> Option.value_map
         ~default:[]
         ~f:(fun ({ repos } : Config.Git_managed_sync_settings.t) -> repos)
  in
  let%map () =
    List.map repos ~f:(fun (repo : Repo.t) -> repo.id)
    |> List.find_a_dup ~compare:String.compare
    |> Option.value_map ~default:(Or_error.return ()) ~f:(fun id ->
      Or_error.error_s [%message "duplicate repo id in config" (id : string)])
    |> Deferred.return
  in
  repos
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Commit everything, pull --rebase, then push after confirmation"
    (let%map_open.Command ids =
       flag
         "id"
         (optional (Nonempty_list.comma_separated_argtype ~strip_whitespace:true string))
         ~doc:"IDS comma-separated config repo ids to sync"
     and dirs =
       flag
         "dir"
         (optional
            (Nonempty_list.comma_separated_argtype
               ~strip_whitespace:true
               File_path.arg_type))
         ~doc:"DIRS comma-separated repo directories to sync"
     and yes = flag "yes" no_arg ~doc:" skip the push confirmation prompt"
     and message =
       flag
         "message"
         (optional_with_default "gkt: auto managed sync" string)
         ~aliases:[ "m" ]
         ~doc:{|MSG commit message (default: "gkt: auto managed sync")|}
     and () = Log.Global.set_level_via_param () in
     fun () ->
       let%bind repos = configured_repos () in
       let%bind dirs =
         (match ids, dirs with
          | None, None ->
            List.map repos ~f:(fun (repo : Repo.t) -> repo.dir)
            |> Nonempty_list.of_list
            |> Or_error.of_option_lazy_sexp
                 ~error:
                   [%lazy_message
                     "no repos to sync: pass -id/-dir or set [git_managed_sync repos] in \
                      the config"]
          | Some ids, None -> resolve_ids ~repos ids
          | None, Some dirs -> Or_error.return dirs
          | Some ids, Some dirs ->
            resolve_ids ~repos ids
            |> Or_error.map ~f:(fun resolved ->
              Nonempty_list.append resolved (Nonempty_list.to_list dirs)))
         |> Deferred.return
       in
       let%bind dirs = expand_home_all dirs |> Deferred.return in
       sync_all ~dirs ~yes ~message)
;;
