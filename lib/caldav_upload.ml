open! Core
open! Async
open Deferred.Or_error.Let_syntax

module Aerc_config = struct
  type t =
    { config_file : File_path.t
    ; config_section : string option
    ; key_source : string
    ; key_cred_cmd : string
    }

  let default_config_file () =
    let xdg = Xdg.create ~env:Sys.getenv () in
    File_path.of_string (Xdg.config_dir xdg ^/ "aerc" ^/ "accounts.conf")
  ;;
end

module Credentials = struct
  type t =
    { server_url : string
    ; username : string option
    ; password : string option
    }

  let resolve_password t ~password_cmd =
    match t.password, password_cmd with
    | Some _, _ | _, None -> return t
    | None, Some cmd ->
      [%log.debug "Running password command"];
      let%bind output = Process.run ~prog:"sh" ~args:[ "-c"; cmd ] () in
      let password = String.strip output in
      if String.is_empty password
      then Deferred.Or_error.error_s [%message "password command produced empty output"]
      else return { t with password = Some password }
  ;;
end

let aerc_source_credentials source_url =
  let uri = Uri.of_string source_url in
  let userinfo_parts =
    Uri.userinfo uri
    |> Option.map ~f:(fun ui ->
      match String.lsplit2 ui ~on:':' with
      | None -> Uri.pct_decode ui, None
      | Some (u, p) -> Uri.pct_decode u, Some (Uri.pct_decode p))
  in
  let server_url = Uri.with_userinfo uri None |> Uri.to_string in
  { Credentials.server_url
  ; username = Option.map userinfo_parts ~f:fst
  ; password = Option.bind userinfo_parts ~f:snd
  }
;;

let redact_url uri = Uri.with_userinfo uri None |> Uri.to_string

let read_aerc_config (aerc : Aerc_config.t)
  : (creds:Credentials.t option * cred_cmd:string option) Deferred.t
  =
  let none = ~creds:None, ~cred_cmd:None in
  let config_file = File_path.to_string aerc.config_file in
  match%bind.Deferred Sys.file_exists config_file with
  | `No | `Unknown -> Deferred.return none
  | `Yes ->
    (match%map.Deferred Ini_file.load aerc.config_file with
     | Error err ->
       [%log.error
         "Failed to parse aerc config" (aerc.config_file : File_path.t) (err : Error.t)];
       none
     | Ok ini ->
       let section_and_source =
         match aerc.config_section with
         | Some sec ->
           Ini_file.get ini ~section:sec ~key:aerc.key_source
           |> Option.map ~f:(fun v -> sec, v)
         | None -> Ini_file.find_key ini ~key:aerc.key_source
       in
       (match section_and_source with
        | None -> none
        | Some (sec, source) ->
          let cred_cmd = Ini_file.get ini ~section:sec ~key:aerc.key_cred_cmd in
          ~creds:(Some (aerc_source_credentials source)), ~cred_cmd))
;;

let non_empty : string option -> string option = Option.filter ~f:(Fn.non String.is_empty)

let resolve_credentials ~server_url ~username ~password ~sexp_config ~aerc =
  let sexp_creds, sexp_password_cmd =
    match (sexp_config : Config.Caldav_upload_settings.t option) with
    | Some c ->
      ( Some
          { Credentials.server_url = c.server_url
          ; username = c.username
          ; password = c.password
          }
      , c.password_cmd )
    | None -> None, None
  in
  Deferred.map
    (read_aerc_config aerc)
    ~f:(fun (~creds:aerc_creds, ~cred_cmd:aerc_cred_cmd) ->
      let aerc_server = Option.map aerc_creds ~f:(fun c -> c.server_url) in
      let aerc_user = Option.bind aerc_creds ~f:(fun c -> c.username) in
      let aerc_pass = Option.bind aerc_creds ~f:(fun c -> c.password) in
      let sexp_server = Option.map sexp_creds ~f:(fun c -> c.server_url) in
      let sexp_user = Option.bind sexp_creds ~f:(fun c -> c.username) in
      let sexp_pass = Option.bind sexp_creds ~f:(fun c -> c.password) in
      let resolved_server =
        List.find_map [ server_url; sexp_server; aerc_server ] ~f:Fn.id |> non_empty
      in
      let resolved_user = List.find_map [ username; sexp_user; aerc_user ] ~f:Fn.id in
      let resolved_pass = List.find_map [ password; sexp_pass; aerc_pass ] ~f:Fn.id in
      let password_cmd = List.find_map [ sexp_password_cmd; aerc_cred_cmd ] ~f:Fn.id in
      match resolved_server with
      | None -> Or_error.error_s [%message "server URL is required"]
      | Some url ->
        Ok
          ( { Credentials.server_url = url
            ; username = resolved_user
            ; password = resolved_pass
            }
          , password_cmd ))
;;

let build_upload_url ~server_url ~uid =
  let uri = Uri.of_string server_url in
  let base_path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
  let filename = [%string "%{Uri.pct_encode ~component:`Generic uid}.ics"] in
  Uri.with_path uri [%string "%{base_path}/%{filename}"]
;;

let http_put_timeout = Time_ns.Span.of_int_sec 30

(* Attach this config to every request and let Cohttp decide whether to use it (it ignores
   [ssl_config] for plaintext connections) rather than re-deciding "is this https?" here
   and risking a different answer than the library.

   The [verify] callback is an *additional* check run after OpenSSL's chain validation.
   async_ssl validates the chain by default (verify_modes defaults to [Verify_peer])
   against the system CA store (set_default_verify_paths, reached because we pass no
   ca_file/ca_path). OpenSSL does not match the hostname against the cert, so we do that
   here, failing closed if it doesn't match. *)
let ssl_config_for_url url =
  Uri.host url
  |> Option.map ~f:(fun hostname ->
    let verify connection =
      match Async_ssl.Ssl.Connection.check_peer_certificate_host connection hostname with
      | Ok () -> Deferred.return true
      | Error err ->
        [%log.error
          "TLS certificate verification failed" (hostname : string) (err : Error.t)];
        Deferred.return false
    in
    Conduit_async.V2.Ssl.Config.create ~hostname ~verify ())
;;

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
  let put_and_body =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      let%bind.Deferred response, body =
        Cohttp_async.Client.put
          ?ssl_config:(ssl_config_for_url url)
          ~headers
          ~chunked:false
          ~body:(`String data)
          url
      in
      let%bind.Deferred body_str = Cohttp_async.Body.to_string body in
      Deferred.return (response, body_str))
  in
  let%bind response, body_str =
    match%bind.Deferred Clock_ns.with_timeout http_put_timeout put_and_body with
    | `Result r -> Deferred.return r
    | `Timeout ->
      Deferred.Or_error.error_s
        [%message
          "CalDAV PUT timed out"
            ~url:(redact_url url : string)
            (http_put_timeout : Time_ns.Span.t)]
  in
  let status_code = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  [%log.debug "CalDAV PUT response" (status_code : int)];
  if 200 <= status_code && status_code < 300
  then return ()
  else
    Deferred.Or_error.error_s
      [%message
        "CalDAV PUT failed"
          (status_code : int)
          ~url:(redact_url url : string)
          ~response_body:body_str]
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
     and aerc_config_file =
       flag
         "aerc-config"
         (optional File_path.arg_type)
         ~aliases:[ "c" ]
         ~doc:"FILE aerc accounts.conf path (fallback credential source)"
     and aerc_section =
       flag
         "aerc-section"
         (optional string)
         ~aliases:[ "S" ]
         ~doc:"SECTION aerc accounts.conf section"
     and aerc_key_source =
       flag
         "aerc-key-source"
         (optional_with_default "caldav-source" string)
         ~doc:"KEY aerc key for CalDAV source URL (default: caldav-source)"
     and aerc_key_cred_cmd =
       flag
         "aerc-key-cred-cmd"
         (optional_with_default "caldav-source-cred-cmd" string)
         ~doc:"KEY aerc key for credential command (default: caldav-source-cred-cmd)"
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
       let aerc : Aerc_config.t =
         { config_file =
             Option.value aerc_config_file ~default:(Aerc_config.default_config_file ())
         ; config_section = aerc_section
         ; key_source = aerc_key_source
         ; key_cred_cmd = aerc_key_cred_cmd
         }
       in
       let%bind creds, password_cmd =
         resolve_credentials
           ~server_url
           ~username
           ~password
           ~sexp_config:(Config.caldav_upload gk_config)
           ~aerc
       in
       let%bind creds = Credentials.resolve_password creds ~password_cmd in
       let url = build_upload_url ~server_url:creds.server_url ~uid in
       let%bind () = http_put ~url ~data:ics_data ~creds ~force in
       [%log.info "Uploaded" (uid : string)];
       return ())
;;
