open! Core
open! Async

module Caldav_upload_settings = struct
  type t =
    { server_url : string
    ; username : string option [@sexp.option]
    ; password : string option [@sexp.option]
    ; password_cmd : string option [@sexp.option]
    }
  [@@deriving sexp]
end

type t = { caldav_upload : Caldav_upload_settings.t option [@sexp.option] }
[@@deriving sexp]

let empty = { caldav_upload = None }
let caldav_upload t = t.caldav_upload

let default_path () =
  let xdg = Xdg.create ~env:Sys.getenv () in
  File_path.of_string (Xdg.config_dir xdg ^/ "gk-tools.sexp")
;;

let load path =
  let path_string = File_path.to_string path in
  match%bind Sys.file_exists path_string with
  | `No | `Unknown -> Deferred.Or_error.return empty
  | `Yes -> Sexp_macro.load_sexp path_string [%of_sexp: t]
;;

let load_default () = load (default_path ())
