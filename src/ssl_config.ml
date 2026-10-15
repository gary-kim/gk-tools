open! Core
open! Async

(* [async_ssl] validates the certificate chain against the system CA store by default
   ([Verify_peer]), but does not check the hostname. *)
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
