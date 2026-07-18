open! Core

let command =
  Command.group
    ~summary:"gk-tools: personal tools binary"
    [ "caldav-upload", Gk_tools.Caldav_upload.command
    ; "git", Gk_tools.Git_commands.command
    ; "misc", Gk_tools.Misc_commands.command
    ; "notmuch", Gk_tools.Notmuch_git.command
    ; "rbw", Gk_tools.Rbw_commands.command
    ]
;;

type build_info =
  { build_host : string
  ; build_user : string
  ; build_time : Time_ns.t
  ; commit_date : string
  ; ocaml_version : string
  }
[@@deriving sexp]

let build_info =
  { build_host = Bin_build_info.build_host
  ; build_user = Bin_build_info.build_user
  ; build_time = Time_ns.of_string Bin_build_info.build_time
  ; commit_date = Bin_build_info.commit_date
  ; ocaml_version = Bin_build_info.ocaml_version
  }
;;

let version =
  let git_hash = Bin_build_info.git_hash in
  let version = Bin_build_info.version in
  [%string "(https://git.sr.ht/~gary-kim/gk-tools/commit/%{git_hash}) %{version}"]
;;

let () =
  Command_unix.run
    command
    ~version
    ~build_info:(sexp_of_build_info build_info |> Sexp.to_string_hum)
;;
