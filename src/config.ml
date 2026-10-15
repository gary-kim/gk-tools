open! Core
open! Async

module Caldav_upload_settings = struct
  type t =
    { server_url : string
    ; username : string option [@sexp.option]
    ; password : Secret_source.t option [@sexp.option]
    }
  [@@deriving sexp, fields ~getters]
end

module Git_managed_sync_settings = struct
  module Repo = struct
    type t =
      { id : string
      ; dir : File_path.t
      }
    [@@deriving sexp]
  end

  type t = { repos : Repo.t list [@sexp.list] } [@@deriving sexp]
end

module Notmuch_git_settings = struct
  type t = { remote : string } [@@deriving sexp]
end

module Llm_completions_settings = struct
  module Reasoning_effort = struct
    type t =
      | None
      | Low
      | Medium
      | High
      | Xhigh
    [@@deriving sexp, string ~capitalize:"snake_case"]
  end

  module Reasoning_summary = struct
    type t =
      | Auto
      | Concise
      | Detailed
    [@@deriving sexp, string ~capitalize:"snake_case"]
  end

  module Url = struct
    include Validated.Make (struct
        include String

        let here = [%here]

        let validate =
          Validate.protect (fun url ->
            Validate.all
              [ Validate.booltest
                  (fun uri ->
                    List.mem
                      [ Some "https"; Some "http" ]
                      (Uri.scheme uri)
                      ~equal:[%equal: string option])
                  ~if_false:"url must use http or https"
              ; Validate.booltest
                  (fun uri -> Option.is_some (Uri.host uri))
                  ~if_false:"url must have a host"
              ; Validate.booltest
                  (fun uri -> Option.is_none (Uri.userinfo uri))
                  ~if_false:"url must not contain credentials"
              ; Validate.booltest
                  (fun uri -> Option.is_none (Uri.fragment uri))
                  ~if_false:"url must not have a fragment"
              ]
              (Uri.of_string url))
        ;;
      end)

    let to_uri t = Uri.of_string (raw t)
  end

  module Model =
    (val String_id.make
           ~caller_identity:
             (Bin_prot.Shape.Uuid.of_string "2127bc4c-d068-42b8-8d87-9189aa179d5b")
           ~module_name:"Llm_completions_settings.Model"
           ~include_default_validation:true
           ())

  module Max_output_tokens = Validated.Make (struct
      include Int

      let here = [%here]
      let validate = Int.validate_lbound ~min:(Incl 16)
    end)

  module Temperature = Validated.Make (struct
      include Float

      let here = [%here]
      let validate = Validate.all [ Float.validate_ordinary; Float.validate_non_negative ]
    end)

  module Timeout = Validated.Make (struct
      include Time_ns.Span

      let here = [%here]
      let validate = Time_ns.Span.validate_positive
    end)

  type t =
    { url : Url.t
    ; token : Secret_source.t option [@sexp.option]
    ; model : Model.t
    ; system_prompt : String.Utf8.t option [@sexp.option]
    ; reasoning_effort : Reasoning_effort.t option [@sexp.option]
    ; reasoning_summary : Reasoning_summary.t option [@sexp.option]
    ; max_output_tokens : Max_output_tokens.t option [@sexp.option]
    ; temperature : Temperature.t option [@sexp.option]
    ; timeout : Timeout.t [@default Timeout.create_exn (Time_ns.Span.of_int_sec 120)]
    }
  [@@deriving sexp]
end

module Classify_mail_settings = struct
  module Destination = struct
    type t =
      { folder : File_path.Relative.t
      ; description : string
      }
    [@@deriving sexp]
  end

  type t =
    { destinations : Destination.t Nonempty_list.t
    ; system_prompt : String.Utf8.t option [@sexp.option]
    }
  [@@deriving sexp]
end

type t =
  { caldav_upload : Caldav_upload_settings.t option [@sexp.option]
  ; git_managed_sync : Git_managed_sync_settings.t option [@sexp.option]
  ; notmuch_git : Notmuch_git_settings.t option [@sexp.option]
  ; llm_completions : Llm_completions_settings.t option [@sexp.option]
  ; classify_mail : Classify_mail_settings.t option [@sexp.option]
  }
[@@deriving sexp, fields ~getters]

let empty =
  { caldav_upload = None
  ; git_managed_sync = None
  ; notmuch_git = None
  ; llm_completions = None
  ; classify_mail = None
  }
;;

let default_path () =
  let xdg = Xdg.create ~env:Sys.getenv () in
  File_path.of_string (Xdg.config_dir xdg ^/ "gk-tools.sexp")
;;

let load path =
  let path_string = File_path.to_string path in
  match%bind Sys.file_exists path_string with
  | `No | `Unknown -> Deferred.Or_error.return empty
  | `Yes -> Sexp_macro.load_sexp path_string [%of_sexp: t]
;;

let load_default () = load (default_path ())
