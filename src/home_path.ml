open! Core

let tilde = File_path.Part.of_string "~"

let chop_tilde relative =
  match File_path.Relative.to_parts relative with
  | first :: rest when File_path.Part.equal first tilde -> Some rest
  | _ -> None
;;

let under_home ~home ~rest =
  File_path.Relative.of_parts rest
  |> Option.value_map ~default:home ~f:(fun rest -> File_path.Absolute.append home rest)
;;

let from_env () =
  Sys.getenv "HOME"
  |> Or_error.of_option ~error:(Error.create_s [%message "$HOME not set"])
  |> Or_error.bind ~f:(fun home ->
    Or_error.try_with (fun () -> File_path.Absolute.of_string home))
;;
