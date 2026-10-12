open! Core
open! Async
open Deferred.Or_error.Let_syntax
open Jsonaf.Export

let max_message_bytes = 100_000

module Max_concurrent_jobs = Validated.Make (struct
    include Int

    let here = [%here]
    let validate = Int.validate_positive
  end)

module Message = struct
  module Message_id = struct
    module Cfws = struct
      type t =
        | Outside
        | Comment of int
        | Quoted_pair of int

      let drop text =
        let offset =
          String.foldi_until
            text
            ~init:Outside
            ~f:(fun index state char ->
              match state, char with
              | Outside, '(' -> Continue (Comment 1)
              | Outside, char when Char.is_whitespace char -> Continue Outside
              | Outside, _ -> Stop index
              | Comment depth, '\\' -> Continue (Quoted_pair depth)
              | Comment depth, '(' -> Continue (Comment (depth + 1))
              | Comment 1, ')' -> Continue Outside
              | Comment depth, ')' -> Continue (Comment (depth - 1))
              | Comment _, _ -> Continue state
              | Quoted_pair depth, _ -> Continue (Comment depth))
            ~finish:(fun index _ -> index)
        in
        String.drop_prefix text offset
      ;;
    end

    let of_header value =
      let parsed =
        let%bind.Option _, rest = String.lsplit2 (Cfws.drop value) ~on:'<' in
        let rest = Cfws.drop rest in
        let id =
          String.lsplit2 rest ~on:'>'
          |> Option.value_map ~default:rest ~f:fst
          |> String.filter ~f:(fun char ->
            not (Char.equal char ' ' || Char.equal char '\t'))
        in
        Option.some_if (not (String.is_empty id)) id
      in
      Option.value parsed ~default:value
    ;;
  end

  type t =
    { raw : string
    ; subject : string option
    ; message_id : string option
    }
  [@@deriving sexp_of]

  let mbox_separator =
    lazy (Re.compile Re.(seq [ bol; str "From "; rep notnl; char '\n' ]))
  ;;

  let is_continuation line =
    String.is_prefix line ~prefix:" " || String.is_prefix line ~prefix:"\t"
  ;;

  let unfolded_headers raw =
    String.split_lines raw
    |> List.take_while ~f:(Fn.non String.is_empty)
    |> List.group ~break:(fun _ next -> not (is_continuation next))
    |> List.map ~f:(String.concat ~sep:"")
  ;;

  let header headers ~name =
    List.find_map headers ~f:(fun line ->
      let%bind.Option key, value = String.lsplit2 line ~on:':' in
      Option.some_if (String.Caseless.equal (String.strip key) name) (String.strip value))
  ;;

  let of_raw raw =
    let%map.Or_error () =
      Result.ok_if_true
        (String.length raw <= max_message_bytes)
        ~error:
          (Error.create_s
             [%message
               "email is too large to classify"
                 ~bytes:(String.length raw : int)
                 (max_message_bytes : int)])
    in
    let headers = unfolded_headers raw in
    { raw
    ; subject = header headers ~name:"Subject"
    ; message_id = header headers ~name:"Message-ID" |> Option.map ~f:Message_id.of_header
    }
  ;;

  let list_of_pipe input =
    (if String.is_prefix input ~prefix:"From "
     then
       Re.split_delim (force mbox_separator) input
       |> List.filter ~f:(Fn.non String.is_empty)
     else [ input ])
    |> List.map ~f:of_raw
  ;;
end

module Classification = struct
  type t =
    { subject : string option
    ; message_id : string option
    ; target_folder : File_path.Relative.t option
    ; reason : String.Utf8.t
    }
  [@@deriving sexp_of]
end

module Target = struct
  type t =
    | No_match
    | Folder of File_path.Relative.t
  [@@deriving sexp_of]
end

module Decision = struct
  module Utf8 = struct
    include String.Utf8
    include functor Jsonaf.Jsonafable.Of_stringable
  end

  type t =
    { reasoning : String.Utf8.t list
    ; target : Target.t
    ; reason : Utf8.t
    }
  [@@deriving sexp_of]

  module Wire = struct
    type t =
      { target_folder : string option
      ; reason : Utf8.t
      }
    [@@deriving of_jsonaf]
  end

  let of_response ~destinations (response : Llm_completions.Response.t) =
    let open Or_error.Let_syntax in
    let%bind wire =
      Jsonaf.parse (String.Utf8.to_string response.content)
      |> Or_error.bind ~f:(fun json ->
        Or_error.try_with (fun () -> Wire.t_of_jsonaf json))
      |> Or_error.tag ~tag:"invalid mail classification JSON"
    in
    let%map target =
      Option.value_map wire.target_folder ~default:(Ok Target.No_match) ~f:(fun name ->
        Nonempty_list.find
          destinations
          ~f:(fun (destination : Config.Classify_mail_settings.Destination.t) ->
            String.equal (File_path.Relative.to_string destination.folder) name)
        |> Or_error.of_option_lazy_sexp
             ~error:[%lazy_message "LLM returned an unknown folder" (name : string)]
        |> Or_error.map ~f:(fun destination -> Target.Folder destination.folder))
    in
    { reasoning = response.reasoning; target; reason = wire.reason }
  ;;
end

module Outcome = struct
  type t =
    { decision : Decision.t
    ; moved : bool
    }
  [@@deriving sexp_of]
end

module Maildir = struct
  let mbsync_uid = lazy (Re.compile Re.(seq [ str ",U="; rep1 digit ]))

  let folder_dir file =
    Option.bind (File_path.Absolute.dirname file) ~f:File_path.Absolute.dirname
  ;;

  let account_root ~folder ~files =
    let roots =
      Nonempty_list.filter_map files ~f:(fun file ->
        Option.bind (folder_dir file) ~f:(File_path.Absolute.chop_suffix ~suffix:folder))
    in
    List.all_equal roots ~equal:File_path.Absolute.equal
    |> Or_error.of_option_lazy_sexp
         ~error:
           [%lazy_message
             "selected email's files don't give one account root for the folder"
               (folder : File_path.Relative.t)
               (files : File_path.Absolute.t Nonempty_list.t)
               ~roots:
                 (List.dedup_and_sort roots ~compare:File_path.Absolute.compare
                  : File_path.Absolute.t list)]
  ;;

  let message_files ~dir message_id =
    let escaped_id = String.substr_replace_all message_id ~pattern:"\"" ~with_:"\"\"" in
    let%bind output =
      Logged_process.run
        ~prog:"notmuch"
        ~args:
          [ "search"
          ; "--output=files"
          ; "--exclude=false"
          ; [%string {|id:"%{escaped_id}"|}]
          ]
        ()
    in
    let%map files =
      Or_error.try_with (fun () ->
        String.split_lines output |> List.map ~f:File_path.Absolute.of_string)
      |> Deferred.return
    in
    List.filter files ~f:(fun file ->
      Option.exists (folder_dir file) ~f:(File_path.Absolute.equal dir))
  ;;

  external rename_noreplace : src:string -> dst:string -> unit = "gkt_rename_noreplace"

  let move ~destination_dir file =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      let name =
        File_path.Absolute.basename_exn file
        |> File_path.Part.to_string
        |> Re.replace_string (force mbsync_uid) ~by:""
        |> File_path.Part.of_string
      in
      let destination =
        File_path.Absolute.append_part
          (File_path.Absolute.append destination_dir (File_path.Relative.of_string "cur"))
          name
      in
      let src = File_path.Absolute.to_string file
      and dst = File_path.Absolute.to_string destination in
      In_thread.syscall_exn ~name:"renameat2" (fun () -> rename_noreplace ~src ~dst))
  ;;
end

let configured_settings () =
  let%bind config = Config.load_default () in
  let%bind settings =
    Config.classify_mail config
    |> Or_error.of_option
         ~error:
           (Error.of_string
              "classify-mail is not configured: set [classify_mail destinations]")
    |> Deferred.return
  and llm_settings =
    Config.llm_completions config
    |> Or_error.of_option
         ~error:
           (Error.of_string "LLM completions are not configured: set [llm_completions]")
    |> Deferred.return
  in
  let%map llm = Llm_completions.create llm_settings in
  settings, llm
;;

let system_prompt (settings : Config.Classify_mail_settings.t) =
  let folders =
    Nonempty_list.map
      settings.destinations
      ~f:(fun ({ folder; description } : Config.Classify_mail_settings.Destination.t) ->
        [%string "%{folder#File_path.Relative}: %{description}"])
    |> Nonempty_list.to_list
    |> String.concat ~sep:"\n"
  in
  let instructions =
    [%string
      {|Classify the email you are given. Reply with exactly one JSON object whose only keys are "target_folder" and "reason". Do not include prose or Markdown fences.
Set "target_folder" to one of these folder names as a string, or to JSON null if none fit:

%{folders}

Choose a folder only when its description matches the email. If none of these folders match, set "target_folder" to null so the email is not moved.

Set "reason" to a short, one-sentence explanation of the classification.|}]
    |> String.Utf8.sanitize
  in
  Option.value_map settings.system_prompt ~default:instructions ~f:(fun system_prompt ->
    String.Utf8.concat [ system_prompt; String.Utf8.of_string "\n\n"; instructions ])
;;

let classify ~llm ~(settings : Config.Classify_mail_settings.t) (message : Message.t) =
  let%bind response =
    Llm_completions.complete
      llm
      ~instructions:(system_prompt settings)
      [ { role = User; content = String.Utf8.sanitize message.raw } ]
  in
  Decision.of_response ~destinations:settings.destinations response |> Deferred.return
;;

let move_to ~root ~folder ~destination_folder (message : Message.t) =
  let%bind message_id =
    Or_error.of_option message.message_id ~error:(Error.of_string "no Message-ID")
    |> Deferred.return
  in
  let%bind files =
    Maildir.message_files ~dir:(File_path.Absolute.append root folder) message_id
  in
  let%bind files =
    Nonempty_list.of_list files
    |> Or_error.of_option_lazy_sexp
         ~error:
           [%lazy_message "no copy of the message in the folder" (message_id : string)]
    |> Deferred.return
  in
  Deferred.Or_error.List.iter
    (Nonempty_list.to_list files)
    ~how:`Sequential
    ~f:(Maildir.move ~destination_dir:(File_path.Absolute.append root destination_folder))
;;

let classify_and_move ~llm ~settings ~root ~folder ~dry_run message =
  let%bind decision = classify ~llm ~settings message in
  let%map moved =
    match decision.target with
    | No_match -> return false
    | Folder destination_folder
      when dry_run || File_path.Relative.equal destination_folder folder -> return false
    | Folder destination_folder ->
      let%map () = move_to ~root ~folder ~destination_folder message in
      true
  in
  { Outcome.decision; moved }
;;

let report ~completed ~total (message : Message.t Or_error.t) result =
  let subject, message_id =
    Option.value_map
      (Result.ok message)
      ~default:(None, None)
      ~f:(fun ({ subject; message_id; _ } : Message.t) -> subject, message_id)
  in
  match result with
  | Error error ->
    [%log.info
      ""
        (completed : int)
        (total : int)
        (subject : string option)
        (message_id : string option)
        (error : Error.t)]
  | Ok ({ decision; moved } : Outcome.t) ->
    let classification =
      { Classification.subject
      ; message_id
      ; target_folder =
          (match decision.target with
           | No_match -> None
           | Folder folder -> Some folder)
      ; reason = decision.reason
      }
    in
    [%log.info
      ""
        (completed : int)
        (total : int)
        (classification : Classification.t)
        (moved : bool)
        ~reasoning:(decision.reasoning : String.Utf8.t list)]
;;

let run ~folder ~files ~max_concurrent_jobs ~dry_run =
  Log.Global.set_output [ Log.Output.stderr ~format:`Sexp_hum () ];
  let%bind root = Maildir.account_root ~folder ~files |> Deferred.return in
  let%bind settings, llm = configured_settings ()
  and input =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Reader.contents (force Reader.stdin))
  in
  let messages = Message.list_of_pipe input in
  let total = List.length messages in
  let completed = ref 0 in
  [%log.info "classifying mail" (dry_run : bool) ~messages:(total : int)];
  Deferred.Or_error.List.iter
    messages
    ~how:(`Max_concurrent_jobs (Max_concurrent_jobs.raw max_concurrent_jobs))
    ~f:(fun message ->
      let%map.Deferred result =
        let%bind message = Deferred.return message in
        classify_and_move ~llm ~settings ~root ~folder ~dry_run message
      in
      Int.incr completed;
      report ~completed:!completed ~total message result;
      Or_error.ignore_m result)
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Classify piped emails with an LLM and move them to configured folders"
    (let%map_open.Command folder =
       flag
         "folder"
         (required File_path.Relative.arg_type)
         ~doc:"FOLDER aerc folder the piped emails are in"
     and files =
       flag
         "file"
         (Nonempty_list.flag
            (Command.Arg_type.map
               File_path.arg_type
               ~f:Filesystem_core.make_absolute_under_cwd))
         ~doc:"FILE a file of the selected email (repeat for each of its files)"
     and max_concurrent_jobs =
       flag
         "jobs"
         (optional_with_default
            (Max_concurrent_jobs.create_exn 8)
            (Command.Arg_type.map int ~f:Max_concurrent_jobs.create_exn))
         ~doc:"N classify up to N emails at once (default: 8)"
     and dry_run =
       flag "dry-run" no_arg ~doc:" classify without moving; log decisions and progress"
     and () = Log.Global.set_level_via_param () in
     fun () -> run ~folder ~files ~max_concurrent_jobs ~dry_run)
;;
