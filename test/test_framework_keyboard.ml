open! Core
open Gk_tools.Framework_keyboard.For_testing

let%expect_test "request layout" =
  List.iter
    [ encode_request ~write:true; encode_request ~write:false ]
    ~f:(fun request ->
      let length = String.length request in
      let zero_padding =
        String.for_all (String.drop_prefix request 32) ~f:(Char.equal '\000')
      in
      print_s [%message (length : int) (zero_padding : bool)];
      print_endline (String.Hexdump.to_string_hum (String.prefix request 32)));
  [%expect
    {|
    ((length 136) (zero_padding true))
    00000000  06 00 00 00 01 00 00 00  01 0c 14 00 02 02 11 00  |................|
    00000010  03 01 ff 00 01 03 1f e0  07 05 58 00 04 04 76 00  |..........X...v.|
    ((length 136) (zero_padding true))
    00000000  06 00 00 00 00 00 00 00  01 0c 00 00 02 02 00 00  |................|
    00000010  03 01 00 00 01 03 00 00  07 05 00 00 04 04 00 00  |................|
    |}]
;;

let%expect_test "readback validation" =
  let update_request = encode_request ~write:true in
  let patch ~pos replacement response =
    let buf = Bytes.of_string response in
    Bytes.set buf pos replacement;
    Bytes.to_string buf
  in
  let check response =
    match verify_readback response with
    | Ok () -> print_endline "ok"
    | Error error ->
      (match Error.sexp_of_t error with
       | List (message :: _) -> print_s message
       | sexp -> print_s sexp)
  in
  update_request |> patch ~pos:4 '\xff' |> patch ~pos:100 '\xff' |> check;
  String.prefix update_request 32 |> check;
  update_request |> patch ~pos:8 '\x02' |> check;
  update_request |> patch ~pos:14 '\xff' |> check;
  [%expect
    {|
    ok
    "short keyboard matrix response"
    "inconsistent keyboard matrix response"
    "keyboard matrix readback mismatch"
    |}]
;;
