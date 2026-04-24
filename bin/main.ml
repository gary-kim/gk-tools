open! Core

let command =
  Command.group
    ~summary:"gk-tools: personal tools binary"
    [ "caldav-upload", Gk_tools.Caldav_upload.command
    ; "rbw-place-secret-files", Gk_tools.Rbw_place_secret_files.command
    ]
;;

let () =
  let version =
    Build_info.V1.version ()
    |> Option.value_map ~default:"dev" ~f:Build_info.V1.Version.to_string
  in
  Command_unix.run command ~version
;;
