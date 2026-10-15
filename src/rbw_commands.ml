open! Core

let command =
  Command.group
    ~summary:"rbw-related subcommands"
    [ "files", Rbw_files.command; "environment", Rbw_environment.command ]
;;
