open! Core
open! Async
open Deferred.Or_error.Let_syntax

let folder = "ENV"
let prefix = "ENV:"

let validate_name name =
  let valid_first c = Char.is_alpha c || Char.equal c '_' in
  let valid_rest c = Char.is_alphanum c || Char.equal c '_' in
  let well_formed =
    (not (String.is_empty name))
    && valid_first name.[0]
    && String.for_all (String.subo name ~pos:1) ~f:valid_rest
  in
  if well_formed
  then Ok name
  else Or_error.error_s [%message "invalid env var name" (name : string)]
;;

let list_env_vars () =
  let%map names = Rbw_cli.search ~folder ~term:prefix in
  List.filter_map names ~f:(String.chop_prefix ~prefix)
;;

let get_env_var ~name =
  Rbw_cli.get_field ~folder ~name:[%string "%{prefix}%{name}"] ~field:"password"
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

let command =
  Command.group
    ~summary:"Manage ENV records via rbw"
    [ "list", list_command; "get", get_command; "export", export_command ]
;;
