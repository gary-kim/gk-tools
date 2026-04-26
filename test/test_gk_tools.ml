open! Core
open! Async

(* Silence Async log output so non-deterministic timestamps don't leak into expect tests
   that exercise [%log] code paths. *)
let () = Log.Global.set_output []

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
    [%sexp (Gk_tools.Ics.parse data |> Or_error.map ~f:(Fn.const ()) : unit Or_error.t)];
  [%expect
    {| (Error ("VCALENDAR missing required properties" (missing (PRODID VERSION)))) |}];
  return ()
;;

(* TODO: the tests below this point were added to verify the line-folding / parser rewrite
   preserves behavior. Once the rewrite settles, prune the redundant ones — we don't need
   every edge case covered forever. *)

let%expect_test "parse - chain of folded continuations" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VEVENT"
      ; "UI"
      ; " D"
      ; " :chain"
      ; " -fold"
      ; " -ed"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| chain-fold-ed |}];
  return ()
;;

let%expect_test "parse - tab continuation" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VEVENT"
      ; "UID:tab"
      ; "\t-folded"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| tab-folded |}];
  return ()
;;

let%expect_test "parse - LF only line endings" =
  let data =
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
UID:lf-only
END:VEVENT
END:VCALENDAR
|}
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| lf-only |}];
  return ()
;;

let%expect_test "parse - input without final newline" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VEVENT"
      ; "UID:no-trailing-newline"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| no-trailing-newline |}];
  return ()
;;

let%expect_test "parse - empty input" =
  print_s
    [%sexp (Gk_tools.Ics.parse "" |> Or_error.map ~f:(Fn.const ()) : unit Or_error.t)];
  [%expect {| (Error "iCalendar data too short") |}];
  return ()
;;

let%expect_test "parse - bare VCALENDAR envelope" =
  let data = "BEGIN:VCALENDAR\r\nEND:VCALENDAR\r\n" in
  print_s
    [%sexp (Gk_tools.Ics.parse data |> Or_error.map ~f:(Fn.const ()) : unit Or_error.t)];
  [%expect
    {| (Error ("VCALENDAR missing required properties" (missing (PRODID VERSION)))) |}];
  return ()
;;

let%expect_test "parse - mismatched BEGIN/END component names" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VEVENT"
      ; "UID:mismatch"
      ; "END:VTODO"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  print_s
    [%sexp (Gk_tools.Ics.parse data |> Or_error.map ~f:(Fn.const ()) : unit Or_error.t)];
  [%expect
    {| (Error ("mismatched BEGIN/END component" (opened VEVENT) (closed VTODO))) |}];
  return ()
;;

let%expect_test "parse - UID inside nested VALARM is ignored" =
  let data =
    String.concat
      ~sep:"\r\n"
      [ "BEGIN:VCALENDAR"
      ; "VERSION:2.0"
      ; "PRODID:-//Test//Test//EN"
      ; "BEGIN:VEVENT"
      ; "UID:event-uid"
      ; "BEGIN:VALARM"
      ; "UID:alarm-uid"
      ; "ACTION:DISPLAY"
      ; "END:VALARM"
      ; "END:VEVENT"
      ; "END:VCALENDAR"
      ; ""
      ]
  in
  let ics = Gk_tools.Ics.parse data |> Or_error.ok_exn in
  print_endline (Gk_tools.Ics.uid ics);
  [%expect {| event-uid |}];
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
    (String.substr_replace_all
       (Gk_tools.Ics.cleaned ics)
       ~pattern:"\r\n"
       ~with_:"\\r\\n\n");
  [%expect
    {|
    BEGIN:VCALENDAR\r\n
    VERSION:2.0\r\n
    PRODID:-//Test//Test//EN\r\n
    X-WR-CALNAME:My Calendar\r\n
    X-WR-TIMEZONE:America/New_York\r\n
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
    (String.substr_replace_all
       (Gk_tools.Ics.cleaned ics)
       ~pattern:"\r\n"
       ~with_:"\\r\\n\n");
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
          (Gk_tools.Ini_file.find_key ini ~key:"caldav-source" : (string * string) option)];
      [%expect {| ((account2 https://cal.work.com/dav)) |}];
      return ())
;;

(* TODO: as with the ICS tests above, the INI tests below were added to verify the rewrite
   preserves behavior. Prune the redundant ones once we're confident in the new
   implementation. *)

let%expect_test "ini_file - empty file" =
  with_temp_ini ~content:"" ~f:(fun path ->
    let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
    print_s [%sexp (Gk_tools.Ini_file.find_key ini ~key:"any" : (string * string) option)];
    [%expect {| () |}];
    return ())
;;

let%expect_test "ini_file - comments only" =
  with_temp_ini ~content:{|# hash comment
; semicolon comment
# another
|} ~f:(fun path ->
    let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
    print_s [%sexp (Gk_tools.Ini_file.find_key ini ~key:"any" : (string * string) option)];
    [%expect {| () |}];
    return ())
;;

let%expect_test "ini_file - bindings before any section" =
  with_temp_ini
    ~content:{|key1 = value1
key2 = value2

[real_section]
key3 = value3
|}
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      print_s
        [%message
          (Gk_tools.Ini_file.get ini ~section:"" ~key:"key1" : string option)
            (Gk_tools.Ini_file.find_key ini ~key:"key2" : (string * string) option)
            (Gk_tools.Ini_file.get ini ~section:"real_section" ~key:"key3"
             : string option)];
      [%expect
        {|
        (("Gk_tools.Ini_file.get ini ~section:\"\" ~key:\"key1\"" (value1))
         ("Gk_tools.Ini_file.find_key ini ~key:\"key2\"" (("" value2)))
         ("Gk_tools.Ini_file.get ini ~section:\"real_section\" ~key:\"key3\""
          (value3)))
        |}];
      return ())
;;

let%expect_test "ini_file - duplicate keys overwrite" =
  with_temp_ini ~content:{|[s]
key = first
key = second
|} ~f:(fun path ->
    let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
    print_s [%sexp (Gk_tools.Ini_file.get ini ~section:"s" ~key:"key" : string option)];
    [%expect {| (second) |}];
    return ())
;;

let%expect_test "ini_file - lines without = are skipped" =
  with_temp_ini
    ~content:{|[s]
key = value
malformed line
another = good
|}
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      print_s
        [%message
          (Gk_tools.Ini_file.get ini ~section:"s" ~key:"key" : string option)
            (Gk_tools.Ini_file.get ini ~section:"s" ~key:"another" : string option)];
      [%expect
        {|
        (("Gk_tools.Ini_file.get ini ~section:\"s\" ~key:\"key\"" (value))
         ("Gk_tools.Ini_file.get ini ~section:\"s\" ~key:\"another\"" (good)))
        |}];
      return ())
;;

let%expect_test "ini_file - = sign in value preserved" =
  with_temp_ini
    ~content:{|[s]
url = https://user:pass@host/path?q=v&r=s
|}
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      print_s [%sexp (Gk_tools.Ini_file.get ini ~section:"s" ~key:"url" : string option)];
      [%expect {| (https://user:pass@host/path?q=v&r=s) |}];
      return ())
;;

let%expect_test "ini_file - whitespace trimming" =
  with_temp_ini
    ~content:"  [  spaced  ]  \n   key   =   value with internal spaces   \n"
    ~f:(fun path ->
      let%bind ini = Gk_tools.Ini_file.load path >>| Or_error.ok_exn in
      print_s
        [%sexp (Gk_tools.Ini_file.get ini ~section:"spaced" ~key:"key" : string option)];
      [%expect {| ("value with internal spaces") |}];
      return ())
;;

let parse_record json =
  Or_error.try_with (fun () ->
    Jsonaf.of_string json |> Gk_tools.Rbw_cli.Record.t_of_jsonaf)
;;

let%expect_test "Rbw_cli.Record - typical record with filepath" =
  let json =
    {|{"id":"x","folder":"SECRET_FILES","name":"foo","data":null,"fields":[{"name":"filepath","value":"~/foo.conf","type":"text"}],"notes":"hello\n","history":[]}|}
  in
  let r = parse_record json |> Or_error.ok_exn in
  print_s
    [%message
      (r.name : string)
        (Gk_tools.Rbw_cli.Record.field r "filepath" : string option)
        (r.notes : string option)];
  [%expect
    {|
    ((r.name foo) ("Gk_tools.Rbw_cli.Record.field r \"filepath\"" (~/foo.conf))
     (r.notes ("hello\n")))
    |}];
  return ()
;;

let%expect_test "Rbw_cli.Record - notes absent" =
  let json =
    {|{"id":"x","folder":"SECRET_FILES","name":"foo","data":null,"fields":[]}|}
  in
  let r = parse_record json |> Or_error.ok_exn in
  print_s [%message (r.name : string) (r.notes : string option)];
  [%expect {| ((r.name foo) (r.notes ())) |}];
  return ()
;;

let%expect_test "Rbw_cli.Record - missing filepath field returns None" =
  let json =
    {|{"id":"x","folder":"SECRET_FILES","name":"foo","data":null,"fields":[{"name":"other","value":"v","type":"text"}],"notes":null}|}
  in
  let r = parse_record json |> Or_error.ok_exn in
  print_s [%sexp (Gk_tools.Rbw_cli.Record.field r "filepath" : string option)];
  [%expect {| () |}];
  return ()
;;

let%expect_test "Rbw_cli.Record - tolerates extra unknown fields" =
  let json =
    {|{"id":"x","folder":"SECRET_FILES","name":"foo","data":null,"fields":[],"notes":"hi","history":[],"future_field":42,"another":"thing"}|}
  in
  let r = parse_record json |> Or_error.ok_exn in
  print_s [%message (r.name : string) (r.notes : string option)];
  [%expect {| ((r.name foo) (r.notes (hi))) |}];
  return ()
;;

let%expect_test "Rbw_files.resolve_target - absolute path" =
  print_endline
    (Gk_tools.Rbw_files.For_testing.resolve_target ~home_dir:"/home/x" "/etc/foo");
  [%expect {| /etc/foo |}];
  return ()
;;

let%expect_test "Rbw_files.resolve_target - tilde-slash expansion" =
  print_endline
    (Gk_tools.Rbw_files.For_testing.resolve_target ~home_dir:"/home/x" "~/foo.conf");
  [%expect {| /home/x/foo.conf |}];
  return ()
;;

let%expect_test "Rbw_files.resolve_target - bare tilde" =
  print_endline (Gk_tools.Rbw_files.For_testing.resolve_target ~home_dir:"/home/x" "~");
  [%expect {| /home/x |}];
  return ()
;;

let%expect_test "Rbw_files.resolve_target - bare relative path" =
  print_endline
    (Gk_tools.Rbw_files.For_testing.resolve_target ~home_dir:"/home/x" "foo.conf");
  [%expect {| /home/x/foo.conf |}];
  return ()
;;

let%expect_test "Rbw_files.resolve - typical record" =
  let record =
    { Gk_tools.Rbw_cli.Record.name = "foo"
    ; fields =
        [ { Gk_tools.Rbw_cli.Field.name = Some "filepath"; value = Some "~/foo.conf" } ]
    ; notes = Some "hello\n"
    }
  in
  print_s
    [%sexp
      (Gk_tools.Rbw_files.For_testing.resolve ~home_dir:"/home/x" record
       : Gk_tools.Rbw_files.For_testing.Resolved.t Or_error.t)];
  [%expect
    {| (Ok ((name foo) (local_target /home/x/foo.conf) (remote_content "hello\n"))) |}];
  return ()
;;

let%expect_test "Rbw_files.resolve - missing filepath" =
  let record =
    { Gk_tools.Rbw_cli.Record.name = "foo"; fields = []; notes = Some "hello" }
  in
  print_s
    [%sexp
      (Gk_tools.Rbw_files.For_testing.resolve ~home_dir:"/home/x" record
       |> Or_error.map ~f:(Fn.const ())
       : unit Or_error.t)];
  [%expect {| (Error ("record missing filepath field" (name foo))) |}];
  return ()
;;

let%expect_test "Rbw_files.resolve - missing notes treated as empty" =
  let record =
    { Gk_tools.Rbw_cli.Record.name = "foo"
    ; fields =
        [ { Gk_tools.Rbw_cli.Field.name = Some "filepath"; value = Some "~/foo.conf" } ]
    ; notes = None
    }
  in
  print_s
    [%sexp
      (Gk_tools.Rbw_files.For_testing.resolve ~home_dir:"/home/x" record
       : Gk_tools.Rbw_files.For_testing.Resolved.t Or_error.t)];
  [%expect {| (Ok ((name foo) (local_target /home/x/foo.conf) (remote_content ""))) |}];
  return ()
;;
