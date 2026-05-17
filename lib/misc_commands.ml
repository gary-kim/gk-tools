open! Core

let command =
  Command.group
    ~summary:"Miscellaneous non-interactive commands"
    [ ( "claude"
      , Command.group
          ~summary:"Claude-related helper commands"
          [ "statusline", Claude_statusline.command ] )
    ]
;;
