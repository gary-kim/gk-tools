open! Core
open! Async

(* The [verify] callback is an *additional* check run after OpenSSL's chain validation.
   async_ssl validates the chain by default (verify_modes defaults to [Verify_peer])
   against the system CA store (set_default_verify_paths, reached because we pass no
   ca_file/ca_path). OpenSSL does not match the hostname against the cert, so we do that
   here, failing closed if it doesn't match. *)
let for_url url =
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
