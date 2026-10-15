open! Core
open! Async

val request
  :  meth:Cohttp.Code.meth
  -> url:Uri.t
  -> headers:Cohttp.Header.t
  -> body:string
  -> timeout:Time_ns.Span.t
  -> string Deferred.Or_error.t
