open! Core
open! Async
open Deferred.Or_error.Let_syntax

let folder = "ENV"
let prefix = "ENV:"

let env_var_name_re =
  lazy
    (let first_char = Re.(alt [ rg 'A' 'Z'; rg 'a' 'z'; char '_' ]) in
     let rest_char = Re.(alt [ first_char; digit ]) in
     Re.compile Re.(whole_string (seq [ first_char; rep rest_char ])))
;;

let validate_name name =
  Result.ok_if_true
    (Re.execp (force env_var_name_re) name)
    ~error:(Error.of_lazy_sexp [%lazy_message "invalid env var name" (name : string)])
  |> Result.map ~f:(fun () -> name)
;;

let list_env_vars () =
  let%map names = Rbw_cli.search ~folder ~term:prefix in
  List.filter_map names ~f:(String.chop_prefix ~prefix)
;;

let get_env_var ~name =
  Rbw_cli.get_field ~folder ~name:[%string "%{prefix}%{name}"] ~field:"password"
;;

let check_no_duplicate_names names =
  Result.ok_if_true
    (not (List.contains_dup (Nonempty_list.to_list names) ~compare:String.compare))
    ~error:(Error.of_lazy_sexp [%lazy_message "duplicate env var name"])
;;

let validate_names names =
  let open Or_error.Let_syntax in
  let%map () =
    Nonempty_list.map names ~f:validate_name
    |> Nonempty_list.to_list
    |> Or_error.combine_errors
    |> Or_error.ignore_m
  and () = check_no_duplicate_names names in
  names
;;

let get_env_vars ~names =
  Deferred.Or_error.List.map
    (Nonempty_list.to_list names)
    ~how:(`Max_concurrent_jobs 4)
    ~f:(fun name ->
      let%map value = get_env_var ~name in
      name, value)
;;

let command_of_escaped escaped =
  escaped
  |> Option.bind ~f:Nonempty_list.of_list
  |> Or_error.of_option_lazy_sexp ~error:[%lazy_message "command missing after --"]
;;

let run_process ~env (command : string Nonempty_list.t) =
  let prog = Nonempty_list.hd command in
  let argv = Nonempty_list.to_list command in
  let%bind pid =
    In_thread.run (fun () ->
      Or_error.try_with (fun () -> Core_unix.fork_exec ~prog ~argv ~env:(`Extend env) ()))
  in
  Unix.waitpid pid |> Deferred.map ~f:Or_error.return
;;

let run_command_with_env ~names command =
  let%bind () = Rbw_cli.sync () in
  let%bind env = get_env_vars ~names in
  run_process ~env command
;;

let name_arg_type =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      Thread_safe.block_on_async_exn (fun () ->
        match%bind.Deferred Rbw_cli.is_unlocked () with
        | false -> Deferred.return []
        | true ->
          (match%map.Deferred list_env_vars () with
           | Error _ -> []
           | Ok names ->
             List.filter names ~f:(fun n -> String.Caseless.is_prefix n ~prefix:part))))
    Fn.id
;;

let name_param =
  let%map_open.Command name = anon ("NAME" %: name_arg_type) in
  validate_name name
;;

let list_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"List available env var names"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind () = Rbw_cli.sync () in
       let%map names = list_env_vars () in
       List.iter names ~f:print_endline;
       ())
;;

let get_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Print the value of an env var"
    (let%map_open.Command name_or_error = name_param in
     fun () ->
       let%bind name = Deferred.return name_or_error in
       let%bind () = Rbw_cli.sync () in
       let%map value = get_env_var ~name in
       print_endline value;
       ())
;;

let export_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Print a shell-eval-able [export NAME='value'] line"
    (let%map_open.Command name_or_error = name_param in
     fun () ->
       let%bind name = Deferred.return name_or_error in
       let%bind () = Rbw_cli.sync () in
       let%map value = get_env_var ~name in
       print_endline [%string "export %{name}=%{Sys.quote value}"];
       ())
;;

let exec_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Run a command with selected env vars from rbw"
    (let%map_open.Command environment =
       flag
         "environment"
         (required
            (Nonempty_list.comma_separated_argtype
               ~strip_whitespace:true
               ~unique_values:true
               name_arg_type))
         ~doc:"NAMES comma-separated env var names to inject, e.g. FOO,BAR"
     (* TODO: use [escape_with_autocomplete] with [compgen -c] for the program slot and
        [compgen -f] for subsequent args so the post-[--] command and its args
        tab-complete. *)
     and command = flag "--" escape ~doc:"COMMAND command and arguments to run" in
     fun () ->
       let%bind names = Deferred.return (validate_names environment) in
       let%bind command = Deferred.return (command_of_escaped command) in
       let%bind status = run_command_with_env ~names command in
       match status with
       | Ok () -> return ()
       | Error (`Exit_non_zero n) ->
         Shutdown.shutdown n;
         Deferred.never ()
       | Error (`Signal s) ->
         Shutdown.shutdown (128 + Signal_unix.to_system_int s);
         Deferred.never ())
;;

let command =
  Command.group
    ~summary:"Manage ENV records via rbw"
    [ "list", list_command
    ; "get", get_command
    ; "export", export_command
    ; "exec", exec_command
    ]
;;
