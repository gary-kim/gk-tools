open! Core
open! Async
open Deferred.Or_error.Let_syntax

module Credentials = struct
  type t =
    { server_url : string
    ; username : string option
    ; password : string option
    }
end

let resolve_credentials ~server_url ~username ~password ~sexp_config =
  let configured_url =
    Option.map sexp_config ~f:Config.Caldav_upload_settings.server_url
  in
  let%bind server_url =
    Option.first_some server_url configured_url
    |> Option.filter ~f:(Fn.non String.is_empty)
    |> Or_error.of_option ~error:(Error.of_string "server URL is required")
    |> Deferred.return
  in
  let username =
    Option.first_some
      username
      (Option.bind sexp_config ~f:Config.Caldav_upload_settings.username)
  in
  let password_source =
    Option.first_some
      (Option.map password ~f:(fun password -> Secret_source.Literal password))
      (Option.bind sexp_config ~f:Config.Caldav_upload_settings.password)
  in
  let%map password =
    Option.value_map password_source ~default:(return None) ~f:(fun source ->
      Secret_source.resolve source
      |> Deferred.Or_error.tag ~tag:"CalDAV password"
      |> Deferred.Or_error.map ~f:Option.some)
  in
  { Credentials.server_url; username; password }
;;

let redact_url uri = Uri.with_userinfo uri None |> Uri.to_string

let build_upload_url ~server_url ~uid =
  let uri = Uri.of_string server_url in
  let base_path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
  let filename = [%string "%{Uri.pct_encode ~component:`Generic uid}.ics"] in
  Uri.with_path uri [%string "%{base_path}/%{filename}"]
;;

let http_put_timeout = Time_ns.Span.of_int_sec 30

let http_put ~url ~data ~(creds : Credentials.t) ~force =
  [%log.debug "CalDAV PUT" ~url:(redact_url url : string)];
  let headers =
    let base =
      Cohttp.Header.init_with "Content-Type" {|text/calendar; charset="utf-8"|}
    in
    let with_auth =
      match creds.username, creds.password with
      | Some u, Some p -> Cohttp.Header.add_authorization base (`Basic (u, p))
      | Some u, None ->
        [%log.error
          "Username provided without password; omitting Authorization header" (u : string)];
        base
      | None, _ -> base
    in
    if force then with_auth else Cohttp.Header.add with_auth "If-None-Match" "*"
  in
  Http_client.request ~meth:`PUT ~url ~headers ~body:data ~timeout:http_put_timeout
  |> Deferred.Or_error.tag_s ~tag:[%message "CalDAV PUT" ~url:(redact_url url : string)]
  |> Deferred.Or_error.ignore_m
;;

module Input_file = struct
  type t =
    | Stdin
    | File of File_path.t

  let arg_type =
    Command.Arg_type.map File_path.arg_type ~f:(fun path ->
      if String.equal (File_path.to_string path) "-" then Stdin else File path)
  ;;
end

let read_ics_data = function
  | Some Input_file.Stdin | None ->
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.contents (Lazy.force Reader.stdin))
  | Some (File path) ->
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.file_contents (File_path.to_string path))
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Upload an iCalendar (.ics) file to a CalDAV server"
    (let%map_open.Command force =
       flag "force" no_arg ~aliases:[ "f" ] ~doc:" overwrite existing event on server"
     and server_url =
       flag
         "server-url"
         (optional string)
         ~aliases:[ "s" ]
         ~doc:"URL CalDAV calendar endpoint (overrides config)"
     and username =
       flag
         "username"
         (optional string)
         ~aliases:[ "u" ]
         ~doc:"USER authentication username (overrides config)"
     and password =
       flag
         "password"
         (optional string)
         ~aliases:[ "p" ]
         ~doc:"PASS authentication password (overrides config)"
     and () = Log.Global.set_level_via_param ()
     and file = anon (maybe ("FILE" %: Input_file.arg_type)) in
     fun () ->
       let%bind ics_data = read_ics_data file in
       let%bind gk_config = Config.load_default () in
       let%bind ics = Ics.parse ics_data |> Deferred.return in
       let uid = Ics.uid ics in
       let ics_data = Ics.cleaned ics in
       let%bind creds =
         resolve_credentials
           ~server_url
           ~username
           ~password
           ~sexp_config:(Config.caldav_upload gk_config)
       in
       let url = build_upload_url ~server_url:creds.server_url ~uid in
       let%bind () = http_put ~url ~data:ics_data ~creds ~force in
       [%log.info "Uploaded" (uid : string)];
       return ())
;;
