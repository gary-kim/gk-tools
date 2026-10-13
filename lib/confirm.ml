open! Core
open! Async

let ask ~yes ~prompt =
  if yes
  then Deferred.Or_error.return true
  else
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.with_file "/dev/tty" ~f:(fun reader ->
        Writer.with_file "/dev/tty" ~append:true ~f:(fun writer ->
          Deferred.repeat_until_finished () (fun () ->
            Writer.write writer [%string "%{prompt} [y/N]: "];
            let%bind () = Writer.flushed writer in
            match%bind Reader.read_line reader with
            | `Eof -> return (`Finished false)
            | `Ok reply ->
              (match String.lowercase (String.strip reply) with
               | "y" | "yes" -> return (`Finished true)
               | "" | "n" | "no" -> return (`Finished false)
               | _ ->
                 Writer.write writer "Please answer yes or no.\n";
                 return (`Repeat ()))))))
    |> Deferred.Or_error.tag
         ~tag:"could not access the terminal for confirmation: pass -yes to skip"
;;

let yes_param =
  Command.Param.flag "yes" Command.Param.no_arg ~doc:" skip the confirmation prompt"
;;
