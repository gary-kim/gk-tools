open! Core
open! Async
open Deferred.Or_error.Let_syntax
open Jsonaf.Export

module Part = struct
  module Content = struct
    module Message = struct
      type 'a t = { body : 'a list } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
    end

    type 'a t =
      | Text of string
      | Parts of 'a list
      | Messages of 'a Message.t list
    [@@deriving of_jsonaf]

    let t_of_jsonaf part_of_jsonaf json =
      let tag =
        match json with
        | `String _ -> "Text"
        | `Array (first :: _) when Option.is_some (Jsonaf.member "body" first) ->
          "Messages"
        | _ -> "Parts"
      in
      t_of_jsonaf part_of_jsonaf (`Array [ `String tag; json ])
    ;;
  end

  type t =
    { id : int
    ; filename : string option [@default None]
    ; content_type : string [@key "content-type"]
    ; content_disposition : string option [@key "content-disposition"] [@default None]
    ; content : t Content.t option [@default None]
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]

  let collect root =
    Sequence.unfold ~init:[ root ] ~f:(fun pending ->
      match pending with
      | [] -> None
      | part :: pending ->
        let children =
          match part.content with
          | None | Some (Text _) -> []
          | Some (Parts parts) -> parts
          | Some (Messages messages) ->
            List.concat_map messages ~f:(fun message -> message.body)
        in
        Some (part, children @ pending))
    |> Sequence.filter ~f:(fun part ->
      Option.is_some part.filename
      || Option.exists part.content_disposition ~f:(String.Caseless.equal "attachment"))
    |> Sequence.to_list
  ;;

  let filename t =
    Option.value_or_thunk t.filename ~default:(fun () ->
      let extension =
        Bool.select (String.Caseless.equal t.content_type "application/pdf") ".pdf" ""
      in
      [%string "attachment-%{t.id#Int}%{extension}"])
  ;;

  module Kind = struct
    type t =
      | Text
      | Image
      | Pdf

    let of_content_type content_type =
      match String.lowercase content_type with
      | "image/png" | "image/jpeg" | "image/webp" | "image/gif" -> Ok Image
      | "application/pdf" -> Ok Pdf
      | mime_type when String.is_prefix mime_type ~prefix:"text/" -> Ok Text
      (* TODO: Add fallback for unrecognized mime types *)
      | mime_type ->
        Or_error.error_s
          [%message "unsupported attachment content type" (mime_type : string)]
    ;;
  end

  module Description = struct
    type t =
      { part_id : int
      ; filename : string
      ; content_type : string
      }
    [@@deriving jsonaf_of]
  end

  let describe t =
    { Description.part_id = t.id; filename = filename t; content_type = t.content_type }
  ;;
end

module Arguments = struct
  type t = { part_id : int option } [@@deriving of_jsonaf]

  let properties =
    let module Schema = Mcp.Protocol.Schema in
    let part_id = Schema.any_of [ Schema.create Integer; Schema.create Null ] in
    String.Map.singleton "part_id" part_id
  ;;
end

let max_attachment_bytes = 1_000_000

let validate_size data =
  let bytes = String.length data in
  let%map.Or_error () =
    Result.ok_if_true
      (bytes <= max_attachment_bytes)
      ~error:
        (Error.create_s
           [%message "attachment is too large" (bytes : int) (max_attachment_bytes : int)])
  in
  data
;;

let fetch_parts ~query =
  let%bind body =
    Logged_process.run
      ~prog:"notmuch"
      ~args:
        [ "show"
        ; "--format=json"
        ; "--format-version=5"
        ; "--part=1"
        ; "--include-html"
        ; "--decrypt=false"
        ; query
        ]
      ()
  in
  let%map root =
    Or_error.try_with (fun () -> Jsonaf.of_string body |> [%of_jsonaf: Part.t])
    |> Deferred.return
  in
  Part.collect root
;;

let listing parts =
  Llm_completions.Tool.Content.text
    (List.map parts ~f:Part.describe
     |> [%jsonaf_of: Part.Description.t list]
     |> Jsonaf.to_string
     |> String.Utf8.sanitize)
;;

let read ~query (part : Part.t) =
  let%bind kind = Part.Kind.of_content_type part.content_type |> Deferred.return in
  let fetch_raw () =
    let%bind mime_type =
      Mcp.Protocol.Mime_type.of_string (String.lowercase part.content_type)
      |> Deferred.return
    in
    let%bind data =
      Logged_process.run
        ~prog:"notmuch"
        ~args:
          [ "show"
          ; "--format=raw"
          ; [%string "--part=%{part.id#Int}"]
          ; "--decrypt=false"
          ; query
          ]
        ()
    in
    let%map data = validate_size data |> Deferred.return in
    mime_type, data
  in
  match (kind : Part.Kind.t) with
  | Text ->
    (match part.content with
     | Some (Text text) ->
       validate_size text
       |> Or_error.map ~f:(fun text ->
         Llm_completions.Tool.Content.text (String.Utf8.sanitize text))
       |> Deferred.return
     | None | Some (Parts _ | Messages _) ->
       Deferred.Or_error.error_string "notmuch returned no attachment text")
  | Image ->
    let%map mime_type, data = fetch_raw () in
    Llm_completions.Tool.Content.image ~mime_type ~data
  | Pdf ->
    let%map mime_type, data = fetch_raw () in
    Llm_completions.Tool.Content.file ~filename:(Part.filename part) ~mime_type ~data
;;

let tool ~message_id =
  let query = Notmuch_query.of_message_id message_id in
  let parts = Lazy_deferred.create_or_error (fun () -> fetch_parts ~query) in
  Llm_completions.Tool.create
    ~name:"get_email_attachment"
    ~description:
      "Read attachments from the email being classified. Call with part_id=null to list \
       attachments and their part IDs, then call with a part ID to read one. Supports \
       text, PDF, PNG, JPEG, WebP, and GIF attachments up to 1 MB."
    ~properties:Arguments.properties
    ~of_jsonaf:[%of_jsonaf: Arguments.t]
    ~f:(fun { Arguments.part_id } ->
      let%bind parts = Lazy_deferred.force parts in
      let%map content =
        match part_id with
        | None -> return (listing parts)
        | Some part_id ->
          let%bind part =
            List.find parts ~f:(fun part -> Int.equal part.Part.id part_id)
            |> Or_error.of_option_lazy_sexp
                 ~error:[%lazy_message "no attachment with this part ID" (part_id : int)]
            |> Deferred.return
          in
          read ~query part
      in
      Nonempty_list.singleton content)
;;
