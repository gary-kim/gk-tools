open! Core

val chop_tilde : File_path.Relative.t -> File_path.Part.t list option

val under_home
  :  home:File_path.Absolute.t
  -> rest:File_path.Part.t list
  -> File_path.Absolute.t

val from_env : unit -> File_path.Absolute.t Or_error.t
