open! Core
open! Async
open Deferred.Or_error.Let_syntax

let request ~meth ~url ~headers ~body ~timeout =
  let request =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      match meth with
      | `HEAD ->
        let%map.Deferred response =
          Cohttp_async.Client.head ?ssl_config:(Ssl_config.for_url url) ~headers url
        in
        response, ""
      | _ ->
        let%bind.Deferred response, body =
          Cohttp_async.Client.call
            ?ssl_config:(Ssl_config.for_url url)
            ~headers
            ~chunked:false
            ~body:(`String body)
            meth
            url
        in
        let%map.Deferred body = Cohttp_async.Body.to_string body in
        response, body)
  in
  let%bind response, body =
    match%bind.Deferred Clock_ns.with_timeout timeout request with
    | `Result result -> Deferred.return result
    | `Timeout ->
      Deferred.Or_error.error_s
        [%message "HTTP request timed out" (timeout : Time_ns.Span.t)]
  in
  let status_code = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  [%log.debug "HTTP response" (status_code : int)];
  if Int.between status_code ~low:200 ~high:299
  then return body
  else
    Deferred.Or_error.error_s
      [%message
        "HTTP request failed" (status_code : int) ~body:(String.prefix body 2048 : string)]
;;
