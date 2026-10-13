open! Core

let command =
  Command.group
    ~summary:"Miscellaneous commands"
    [ "classify-mail", Classify_mail.command
    ; ( "claude"
      , Command.group
          ~summary:"Claude-related helper commands"
          [ "statusline", Claude_statusline.command ] )
    ]
;;
