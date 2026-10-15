open! Core
open Jsonaf.Export

let protocol_version = "2025-11-25"

module Mime_type = struct
  type t = string [@@deriving sexp_of]

  (* TODO: Support media-type parameters. *)
  let of_string value =
    let token s =
      (not (String.is_empty s))
      && String.for_all s ~f:(fun c ->
        Char.is_alphanum c || String.mem "!#$%&'*+-.^_`|~" c)
    in
    Result.ok_if_true
      (Option.value_map
         (String.lsplit2 value ~on:'/')
         ~default:false
         ~f:(fun (major, minor) -> token major && token minor))
      ~error:(Error.create_s [%message "Invalid MIME type" (value : string)])
    |> Or_error.map ~f:(fun () -> value)
  ;;

  let to_string = Fn.id
  let jsonaf_of_t = jsonaf_of_string
  let t_of_jsonaf json = string_of_jsonaf json |> of_string |> Or_error.ok_exn
end

module Json_object = struct
  type 'a t = 'a String.Map.t [@@deriving sexp_of]

  let jsonaf_of_t convert t =
    `Object (Map.to_alist t |> List.map ~f:(fun (key, value) -> key, convert value))
  ;;

  let t_of_jsonaf convert json =
    Jsonaf.assoc_list json
    |> Or_error.of_option ~error:(Error.of_string "expected a JSON object")
    |> Or_error.bind ~f:(fun fields ->
      List.map fields ~f:(fun (key, value) -> key, convert value)
      |> Map.of_alist_or_error (module String))
    |> Or_error.ok_exn
  ;;
end

module Schema = struct
  module Kind = struct
    type t =
      | String
      | Number
      | Integer
      | Boolean
      | Object
      | Array
      | Null
    [@@deriving sexp_of, equal, string ~capitalize:"snake_case"]

    include functor Jsonaf.Jsonafable.Of_stringable
  end

  (* TODO: Preserve JSON Schema keywords beyond this record. *)
  type t =
    { kind : Kind.t option [@key "type"] [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; properties : t Json_object.t option [@jsonaf.option]
    ; required : string list option [@jsonaf.option]
    ; additional_properties : bool option [@key "additionalProperties"] [@jsonaf.option]
    ; items : t option [@jsonaf.option]
    ; enum : Jsonaf.t list option [@jsonaf.option]
    ; any_of : t list option [@key "anyOf"] [@jsonaf.option]
    ; ref_ : string option [@key "$ref"] [@jsonaf.option]
    }
  [@@deriving sexp_of, jsonaf]

  let create ?description ?enum kind =
    { kind = Some kind
    ; description
    ; properties = None
    ; required = None
    ; additional_properties = None
    ; items = None
    ; enum
    ; any_of = None
    ; ref_ = None
    }
  ;;

  let array ?description items = { (create ?description Array) with items = Some items }

  module Object = struct
    type nonrec t = t [@@deriving sexp_of, jsonaf_of]

    let create
      ?description
      ?(properties = String.Map.empty)
      ?(required = [])
      ?additional_properties
      ()
      =
      { (create ?description Object) with
        properties = Some properties
      ; required = Some required
      ; additional_properties
      }
    ;;

    let to_schema = Fn.id
  end
end

module Implementation = struct
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp_of, jsonaf] [@@jsonaf.allow_extra_fields]
end

module Rpc_error = struct
  type t =
    { code : int
    ; message : string
    ; data : Jsonaf.t option [@jsonaf.option]
    }
  [@@deriving sexp_of, jsonaf_of]

  let create ?data ~code message = { code; message; data }
  let invalid_params message = create ~code:(-32602) message
  let internal error = create ~code:(-32603) (Error.to_string_hum error)
end

module Request = struct
  module Id = struct
    type t =
      | String of string
      | Int of int
    [@@deriving sexp_of, compare, hash]

    let of_jsonaf json =
      match Jsonaf.string json, Jsonaf.int json with
      | Some value, _ -> Ok (String value)
      | _, Some value -> Ok (Int value)
      | _ -> Or_error.error_string "request ID must be a string or integer"
    ;;

    let jsonaf_of_t = function
      | String value -> jsonaf_of_string value
      | Int value -> jsonaf_of_int value
    ;;

    let t_of_jsonaf json = of_jsonaf json |> Or_error.ok_exn
  end

  module Version = struct
    type t = string [@@deriving jsonaf_of]

    let t_of_jsonaf json =
      let version = string_of_jsonaf json in
      if not (String.equal version "2.0") then raise_s [%message "Expected JSON-RPC 2.0"];
      version
    ;;
  end

  type t =
    { jsonrpc : Version.t
    ; id : Id.t option [@jsonaf.option]
    ; method_ : string [@key "method"]
    ; params : Jsonaf.t Json_object.t option [@jsonaf.option]
    }
  [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

  let of_jsonaf json = Or_error.try_with (fun () -> t_of_jsonaf json)
end

module Response = struct
  module Success = struct
    type t =
      { jsonrpc : string
      ; id : Request.Id.t
      ; result : Jsonaf.t
      }
    [@@deriving jsonaf_of]
  end

  module Failure = struct
    type t =
      { jsonrpc : string
      ; id : Request.Id.t option [@jsonaf.option]
      ; error : Rpc_error.t
      }
    [@@deriving jsonaf_of]
  end

  let result ~id result = Success.jsonaf_of_t { jsonrpc = "2.0"; id; result }
  let error ~id error = Failure.jsonaf_of_t { jsonrpc = "2.0"; id; error }
end

module Content = struct
  module Kind = struct
    type t =
      | Text
      | Image
      | Audio
    [@@deriving string ~capitalize:"snake_case"]

    include functor Jsonaf.Jsonafable.Of_stringable
  end

  module Text = struct
    type t = { text : string } [@@deriving sexp_of, jsonaf_of]

    module Wire = struct
      type t =
        { type_ : Kind.t [@key "type"]
        ; text : string
        }
      [@@deriving jsonaf_of]
    end
  end

  module Binary = struct
    type t =
      { data : string
      ; mime_type : Mime_type.t [@key "mimeType"]
      }
    [@@deriving sexp_of, jsonaf_of]

    module Wire = struct
      type t =
        { type_ : Kind.t [@key "type"]
        ; data : string
        ; mime_type : Mime_type.t [@key "mimeType"]
        }
      [@@deriving jsonaf_of]
    end
  end

  type t =
    | Text of Text.t
    | Image of Binary.t
    | Audio of Binary.t
  [@@deriving sexp_of]

  let jsonaf_of_t t =
    match t with
    | Text { text } -> Text.Wire.jsonaf_of_t { type_ = Text; text }
    | Image { data; mime_type } ->
      Binary.Wire.jsonaf_of_t { type_ = Image; data; mime_type }
    | Audio { data; mime_type } ->
      Binary.Wire.jsonaf_of_t { type_ = Audio; data; mime_type }
  ;;

  let text text = Text { text }
end

module Tool = struct
  type t =
    { name : string
    ; description : string option [@jsonaf.option]
    ; input_schema : Schema.Object.t [@key "inputSchema"]
    ; output_schema : Schema.Object.t option [@key "outputSchema"] [@jsonaf.option]
    }
  [@@deriving sexp_of, jsonaf_of]

  module Result = struct
    type t =
      { content : Content.t list
      ; structured_content : Jsonaf.t Json_object.t option
           [@key "structuredContent"] [@jsonaf.option]
      ; is_error : bool [@key "isError"]
      }
    [@@deriving sexp_of, jsonaf_of]

    let text ?(is_error = false) text =
      { content = [ Content.text text ]; structured_content = None; is_error }
    ;;
  end
end

module Resource = struct
  type t =
    { uri : string
    ; name : string
    ; description : string option [@jsonaf.option]
    ; mime_type : Mime_type.t option [@key "mimeType"] [@jsonaf.option]
    }
  [@@deriving sexp_of, jsonaf_of]

  module Contents = struct
    module Text = struct
      type t =
        { uri : string
        ; mime_type : Mime_type.t option [@key "mimeType"] [@jsonaf.option]
        ; text : string
        }
      [@@deriving sexp_of, jsonaf_of]
    end

    module Blob = struct
      type t =
        { uri : string
        ; mime_type : Mime_type.t option [@key "mimeType"] [@jsonaf.option]
        ; blob : string
        }
      [@@deriving sexp_of, jsonaf_of]
    end

    type t =
      | Text of Text.t
      | Blob of Blob.t
    [@@deriving sexp_of]

    let jsonaf_of_t = function
      | Text text -> Text.jsonaf_of_t text
      | Blob blob -> Blob.jsonaf_of_t blob
    ;;
  end
end

module Prompt = struct
  module Argument = struct
    type t =
      { name : string
      ; description : string option [@jsonaf.option]
      ; required : bool
      }
    [@@deriving sexp_of, jsonaf_of]
  end

  type t =
    { name : string
    ; description : string option [@jsonaf.option]
    ; arguments : Argument.t list
    }
  [@@deriving sexp_of, jsonaf_of]

  module Message = struct
    module Role = struct
      type t =
        | User
        | Assistant
      [@@deriving sexp_of, string ~capitalize:"snake_case"]

      include functor Jsonaf.Jsonafable.Of_stringable
    end

    type t =
      { role : Role.t
      ; content : Content.t
      }
    [@@deriving sexp_of, jsonaf_of]
  end

  module Result = struct
    type t =
      { description : string option [@jsonaf.option]
      ; messages : Message.t list
      }
    [@@deriving sexp_of, jsonaf_of]
  end
end
