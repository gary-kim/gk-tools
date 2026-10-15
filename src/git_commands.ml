open! Core

let command =
  Command.group
    ~summary:"git-related subcommands"
    [ "managed-sync", Git_managed_sync.command ]
;;
