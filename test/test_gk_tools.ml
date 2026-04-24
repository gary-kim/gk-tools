open! Core
open! Async
module Expect_test_config = Async.Expect_test_config

let%expect_test "parse - simple UID extraction" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:test-123@example.com\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| test-123@example.com |}];
  return ()
;;

let%expect_test "parse - folded UID line" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UI\r\n\
    \ D:folded-uid\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| folded-uid |}];
  return ()
;;

let%expect_test "parse - UID with parameters" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID;VALUE=TEXT:param-uid@example.com\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| param-uid@example.com |}];
  return ()
;;

let%expect_test "parse - no UID" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     SUMMARY:test\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  print_s [%sexp (Gk_tools.Ics.parse data |> Or_error.is_error : bool)];
  [%expect {| true |}];
  return ()
;;

let%expect_test "parse - multiple UIDs" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:uid-1\r\n\
     END:VEVENT\r\n\
     BEGIN:VEVENT\r\n\
     UID:uid-2\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  print_s [%sexp (Gk_tools.Ics.parse data |> Or_error.is_error : bool)];
  [%expect {| true |}];
  return ()
;;

let%expect_test "parse - missing VERSION and PRODID" =
  let data =
    "BEGIN:VCALENDAR\r\n\
     BEGIN:VEVENT\r\n\
     UID:test-123@example.com\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  print_s
    [%sexp
      (Gk_tools.Ics.parse data |> Or_error.map ~f:(Fn.const ()) : unit Or_error.t)];
  [%expect
    {| (Error ("VCALENDAR missing required properties" (missing (PRODID VERSION)))) |}];
  return ()
;;

let%expect_test "cleaned - strips non-whitelisted VCALENDAR properties" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "X-WR-CALNAME:My Calendar"
      ; "X-WR-TIMEZONE:America/New_York"
      ; "CALSCALE:GREGORIAN"
      ; "BEGIN:VEVENT"
      ; "UID:test@example.com"
      ; "SUMMARY:Test Event"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_string
    (String.substr_replace_all (Gk_tools.Ics.cleaned ics) ~pattern:"\r\n" ~with_:"\\r\\n\n");
  [%expect
    {|
    BEGIN:VCALENDAR\r\n
    VERSION:2.0\r\n
    PRODID:-//Test//Test//EN\r\n
    CALSCALE:GREGORIAN\r\n
    BEGIN:VEVENT\r\n
    UID:test@example.com\r\n
    SUMMARY:Test Event\r\n
    END:VEVENT\r\n
    END:VCALENDAR\r\n
    |}];
  return ()
;;

let%expect_test "cleaned - preserves all component content" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VTIMEZONE"
      ; "TZID:America/New_York"
      ; "BEGIN:STANDARD"
      ; "DTSTART:19701101T020000"
      ; "END:STANDARD"
      ; "END:VTIMEZONE"
      ; "BEGIN:VEVENT"
      ; "UID:tz-test@example.com"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_string
    (String.substr_replace_all (Gk_tools.Ics.cleaned ics) ~pattern:"\r\n" ~with_:"\\r\\n\n");
  [%expect
    {|
    BEGIN:VCALENDAR\r\n
    VERSION:2.0\r\n
    PRODID:-//Test//Test//EN\r\n
    BEGIN:VTIMEZONE\r\n
    TZID:America/New_York\r\n
    BEGIN:STANDARD\r\n
    DTSTART:19701101T020000\r\n
    END:STANDARD\r\n
    END:VTIMEZONE\r\n
    BEGIN:VEVENT\r\n
    UID:tz-test@example.com\r\n
    END:VEVENT\r\n
    END:VCALENDAR\r\n
    |}];
  return ()
;;

let with_temp_ini ~content ~f =
  Filesystem_async.with_temp_file ~prefix:"test_ini" (fun path ->
    let path = File_path.Absolute.to_string path in
    let%bind () = Writer.save path ~contents:content in
    f path)
;;

let%expect_test "ini_file - basic read and lookup" =
  with_temp_ini
    ~content:
      {|[section1]
key1 = value1
caldav-source = https://user:pass@cal.example.com/dav/cal

[section2]
key2 = value2
|}
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      let get ~key =
        Gk_tools.Ini_file.get ini ~section:"section1" ~key
        |> Option.value ~default:"<none>"
      in
      print_s
        [%message
          (get ~key:"key1" : string)
            (get ~key:"caldav-source" : string)
            (get ~key:"missing" : string)];
      [%expect
        {|
        (("get ~key:\"key1\"" value1)
         ("get ~key:\"caldav-source\"" https://user:pass@cal.example.com/dav/cal)
         ("get ~key:\"missing\"" <none>))
        |}];
      return ())
;;

let%expect_test "ini_file - find_key across sections" =
  with_temp_ini
    ~content:
      {|[account1]
name = personal

[account2]
name = work
caldav-source = https://cal.work.com/dav
caldav-source-cred-cmd = rbw get caldav
|}
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      print_s
        [%sexp
          (Gk_tools.Ini_file.find_key ini ~key:"caldav-source"
            : (string * string) option)];
      [%expect {| ((account2 https://cal.work.com/dav)) |}];
      return ())
;;
