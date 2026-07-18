open! Core

let chop_tilde path =
  if String.equal path "~" then Some "" else String.chop_prefix path ~prefix:"~/"
;;

let under_home ~home ~rest = if String.is_empty rest then home else home ^/ rest
