open! Core
open! Async
open Deferred.Or_error.Let_syntax

let device = "/dev/cros_ec"
let entries_offset = 8
let entry_size = 4
let max_entries = 32
let payload_size = entries_offset + (max_entries * entry_size)

module Position = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving equal, sexp_of]

  let left_ctrl = { row = 1; col = 12 }
  let fn = { row = 2; col = 2 }
  let left_super = { row = 3; col = 1 }
  let left_alt = { row = 1; col = 3 }
  let escape = { row = 7; col = 5 }
  let caps_lock = { row = 4; col = 4 }
end

module Scancode = struct
  type t = int [@@deriving equal]

  let sexp_of_t = Int.Hex.sexp_of_t
  let left_ctrl = 0x0014
  let left_alt = 0x0011
  let fn = 0x00ff
  let left_super = 0xe01f
  let escape = 0x0076
  let caps_lock = 0x0058
end

module Entry = struct
  type t =
    { position : Position.t
    ; scancode : Scancode.t
    }
  [@@deriving equal, sexp_of]
end

module Response = struct
  type t =
    { ec_result : int
    ; payload : string
    }
end

module Outcome = struct
  type t =
    | Unchanged
    | Remapped of { previous : Entry.t list }
end

external keyboard_matrix_command_exn
  :  Core_unix.File_descr.t
  -> string
  -> Response.t
  = "gkt_ec_update_keyboard_matrix"

let target : Entry.t list =
  [ { position = Position.left_ctrl; scancode = Scancode.left_ctrl }
  ; { position = Position.fn; scancode = Scancode.left_alt }
  ; { position = Position.left_super; scancode = Scancode.fn }
  ; { position = Position.left_alt; scancode = Scancode.left_super }
  ; { position = Position.escape; scancode = Scancode.caps_lock }
  ; { position = Position.caps_lock; scancode = Scancode.escape }
  ]
;;

let encode_request ~write =
  let buf = Bytes.make payload_size '\000' in
  Binary_packing.pack_unsigned_32_int_little_endian ~buf ~pos:0 (List.length target);
  Binary_packing.pack_unsigned_32_int_little_endian ~buf ~pos:4 (Bool.to_int write);
  List.iteri target ~f:(fun i { Entry.position = { row; col }; scancode } ->
    let pos = entries_offset + (i * entry_size) in
    Binary_packing.pack_unsigned_8 ~buf ~pos row;
    Binary_packing.pack_unsigned_8 ~buf ~pos:(pos + 1) col;
    if write
    then Binary_packing.pack_unsigned_16_little_endian ~buf ~pos:(pos + 2) scancode);
  Bytes.to_string buf
;;

let decode_response payload =
  let received = String.length payload in
  let%bind.Or_error () =
    Result.ok_if_true
      (received = payload_size)
      ~error:
        (Error.of_lazy_sexp
           [%lazy_message "short keyboard matrix response" (received : int)])
  in
  let buf = Bytes.of_string payload in
  let num_items = Binary_packing.unpack_unsigned_32_int_little_endian ~buf ~pos:0 in
  let entries =
    List.init (List.length target) ~f:(fun i ->
      let pos = entries_offset + (i * entry_size) in
      { Entry.position =
          { row = Binary_packing.unpack_unsigned_8 ~buf ~pos
          ; col = Binary_packing.unpack_unsigned_8 ~buf ~pos:(pos + 1)
          }
      ; scancode = Binary_packing.unpack_unsigned_16_little_endian ~buf ~pos:(pos + 2)
      })
  in
  let%map.Or_error () =
    Result.ok_if_true
      (num_items = List.length target
       && List.equal
            (Comparable.lift Position.equal ~f:(fun entry -> entry.Entry.position))
            entries
            target)
      ~error:
        (Error.of_lazy_sexp
           [%lazy_message
             "inconsistent keyboard matrix response"
               (num_items : int)
               (entries : Entry.t list)])
  in
  entries
;;

let verify readback =
  Result.ok_if_true
    ([%equal: Entry.t list] readback target)
    ~error:
      (Error.of_lazy_sexp
         [%lazy_message
           "keyboard matrix readback mismatch"
             ~expected:(target : Entry.t list)
             ~actual:(readback : Entry.t list)])
;;

let verify_readback payload = decode_response payload |> Or_error.bind ~f:verify

let check_ec_result ec_result =
  match ec_result with
  | 0 -> Ok ()
  | 1 -> Or_error.error_s [%message "EC does not support the keyboard matrix command"]
  | ec_result ->
    Or_error.error_s
      [%message "EC rejected the keyboard matrix command" (ec_result : int)]
;;

let transact fd request =
  let%bind.Or_error { Response.ec_result; payload } =
    Or_error.try_with (fun () -> keyboard_matrix_command_exn fd request)
  in
  let%map.Or_error () = check_ec_result ec_result in
  payload
;;

let remap fd =
  let query_request = encode_request ~write:false in
  let%bind.Or_error previous =
    transact fd query_request
    |> Or_error.bind ~f:decode_response
    |> Or_error.tag ~tag:"querying keyboard matrix before update"
  in
  if [%equal: Entry.t list] previous target
  then Ok Outcome.Unchanged
  else (
    let update_request = encode_request ~write:true in
    let%bind.Or_error reply =
      transact fd update_request |> Or_error.tag ~tag:"updating keyboard matrix"
    in
    let%map.Or_error () =
      verify_readback reply |> Or_error.tag ~tag:"validating keyboard matrix update reply"
    and () =
      transact fd query_request
      |> Or_error.bind ~f:verify_readback
      |> Or_error.tag ~tag:"reading back keyboard matrix after update"
    in
    Outcome.Remapped { previous })
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:
      "Set Framework 13 Pro (Sakura) modifiers to [Ctrl Alt Fn Super] and swap \
       Escape/Caps Lock"
    ~readme:(fun () -> "FYI, this will also put fn-lock on the caps lock key")
    (let%map_open.Command () = Log.Global.set_level_via_param () in
     fun () ->
       match%map
         In_thread.run (fun () ->
           Or_error.try_with_join (fun () ->
             Core_unix.with_file device ~mode:[ O_RDWR; O_CLOEXEC ] ~f:remap))
       with
       | Unchanged -> ()
       | Remapped { previous } ->
         [%log.info "remapped keyboard keys" (previous : Entry.t list)])
;;

module For_testing = struct
  let encode_request = encode_request
  let verify_readback = verify_readback
end
