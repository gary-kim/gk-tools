open! Core

let of_message_id message_id =
  let escaped_id = String.substr_replace_all message_id ~pattern:"\"" ~with_:"\"\"" in
  [%string {|id:"%{escaped_id}"|}]
;;
