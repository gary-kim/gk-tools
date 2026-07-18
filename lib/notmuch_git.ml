open! Core
open! Async
open Deferred.Or_error.Let_syntax

let check_no_dash_prefix ~label arg =
  if String.is_prefix arg ~prefix:"-"
  then
    Or_error.error_s
      [%message
        "notmuch-git argument may not start with [-]" (label : string) (arg : string)]
  else Ok arg
;;

let notmuch_git args = Logged_process.run ~prog:"notmuch-git" ~args ()

let configured_settings () =
  let%bind config = Config.load_default () in
  Config.notmuch_git config
  |> Or_error.of_option_lazy_sexp
       ~error:[%lazy_message "notmuch-git is not configured: set [notmuch_git remote]"]
  |> Deferred.return
;;

let setup () =
  let%bind ({ remote } : Config.Notmuch_git_settings.t) = configured_settings () in
  let%bind (_ : string) =
    check_no_dash_prefix ~label:"remote" remote |> Deferred.return
  in
  let%bind (_ : string) = notmuch_git [ "clone"; remote ] in
  [%log.info "cloned" (remote : string)];
  let%map (_ : string) = notmuch_git [ "checkout"; "--force" ] in
  [%log.info "checked out tags into the notmuch database"]
;;

let sync ~message ~force =
  let%bind (_ : string) =
    check_no_dash_prefix ~label:"message" message |> Deferred.return
  in
  let%bind (_ : string) =
    notmuch_git (("commit" :: (if force then [ "--force" ] else [])) @ [ message ])
  in
  [%log.info "committed"];
  let%bind (_ : string) = notmuch_git [ "pull" ] in
  [%log.info "pulled"];
  let%map (_ : string) = notmuch_git [ "push" ] in
  [%log.info "pushed"]
;;

let setup_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Clone the configured notmuch-git tag repo and check it out on this box"
    (let%map_open.Command () = Log.Global.set_level_via_param () in
     fun () -> setup ())
;;

let sync_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Run the notmuch-git commit, pull, push sequence"
    (let%map_open.Command message =
       flag
         "message"
         (optional_with_default "gkt: auto notmuch sync" string)
         ~aliases:[ "m" ]
         ~doc:{|MSG commit message (default: "gkt: auto notmuch sync")|}
     and force =
       flag
         "force"
         no_arg
         ~doc:" override notmuch-git's large-change safety check when committing"
     and () = Log.Global.set_level_via_param () in
     fun () -> sync ~message ~force)
;;

let command =
  Command.group
    ~summary:"Manage notmuch tag syncing via notmuch-git"
    [ "setup", setup_command; "sync", sync_command ]
;;
