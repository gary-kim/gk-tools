open! Core

val protocol_version : string

module Mime_type : sig
  type t [@@deriving sexp_of, jsonaf]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Json_object : sig
  type 'a t = 'a String.Map.t [@@deriving sexp_of, jsonaf]
end

module Schema : sig
  module Kind : sig
    type t =
      | String
      | Number
      | Integer
      | Boolean
      | Object
      | Array
      | Null
    [@@deriving sexp_of, equal, string, jsonaf]
  end

  type t =
    { kind : Kind.t option
    ; description : string option
    ; properties : t Json_object.t option
    ; required : string list option
    ; additional_properties : bool option
    ; items : t option
    ; enum : Jsonaf.t list option
    ; any_of : t list option
    ; ref_ : string option
    }
  [@@deriving sexp_of, jsonaf]

  val create : ?description:string -> ?enum:Jsonaf.t list -> Kind.t -> t
  val array : ?description:string -> t -> t

  module Object : sig
    type schema := t
    type t [@@deriving sexp_of, jsonaf_of]

    val create
      :  ?description:string
      -> ?properties:schema Json_object.t
      -> ?required:string list
      -> ?additional_properties:bool
      -> unit
      -> t

    val to_schema : t -> schema
  end
end

module Implementation : sig
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp_of, jsonaf]
end

module Rpc_error : sig
  type t =
    { code : int
    ; message : string
    ; data : Jsonaf.t option
    }
  [@@deriving sexp_of, jsonaf_of]

  val create : ?data:Jsonaf.t -> code:int -> string -> t
  val invalid_params : string -> t
  val internal : Error.t -> t
end

module Request : sig
  module Id : sig
    type t =
      | String of string
      | Int of int
    [@@deriving sexp_of, compare, hash, jsonaf]

    val of_jsonaf : Jsonaf.t -> t Or_error.t
  end

  module Version : sig
    type t = string [@@deriving jsonaf]
  end

  type t =
    { jsonrpc : Version.t
    ; id : Id.t option
    ; method_ : string
    ; params : Jsonaf.t Json_object.t option
    }
  [@@deriving jsonaf]

  val of_jsonaf : Jsonaf.t -> t Or_error.t
end

module Response : sig
  val result : id:Request.Id.t -> Jsonaf.t -> Jsonaf.t
  val error : id:Request.Id.t option -> Rpc_error.t -> Jsonaf.t
end

module Content : sig
  module Text : sig
    type t = { text : string } [@@deriving sexp_of, jsonaf_of]
  end

  module Binary : sig
    type t =
      { data : string
      ; mime_type : Mime_type.t
      }
    [@@deriving sexp_of, jsonaf_of]
  end

  type t =
    | Text of Text.t
    | Image of Binary.t
    | Audio of Binary.t
  [@@deriving sexp_of, jsonaf_of]

  val text : string -> t
end

module Tool : sig
  type t =
    { name : string
    ; description : string option
    ; input_schema : Schema.Object.t
    ; output_schema : Schema.Object.t option
    }
  [@@deriving sexp_of, jsonaf_of]

  module Result : sig
    type t =
      { content : Content.t list
      ; structured_content : Jsonaf.t Json_object.t option
      ; is_error : bool
      }
    [@@deriving sexp_of, jsonaf_of]

    val text : ?is_error:bool -> string -> t
  end
end

module Resource : sig
  type t =
    { uri : string
    ; name : string
    ; description : string option
    ; mime_type : Mime_type.t option
    }
  [@@deriving sexp_of, jsonaf_of]

  module Contents : sig
    module Text : sig
      type t =
        { uri : string
        ; mime_type : Mime_type.t option
        ; text : string
        }
      [@@deriving sexp_of, jsonaf_of]
    end

    module Blob : sig
      type t =
        { uri : string
        ; mime_type : Mime_type.t option
        ; blob : string
        }
      [@@deriving sexp_of, jsonaf_of]
    end

    type t =
      | Text of Text.t
      | Blob of Blob.t
    [@@deriving sexp_of, jsonaf_of]
  end
end

module Prompt : sig
  module Argument : sig
    type t =
      { name : string
      ; description : string option
      ; required : bool
      }
    [@@deriving sexp_of, jsonaf_of]
  end

  type t =
    { name : string
    ; description : string option
    ; arguments : Argument.t list
    }
  [@@deriving sexp_of, jsonaf_of]

  module Message : sig
    module Role : sig
      type t =
        | User
        | Assistant
      [@@deriving sexp_of, string, jsonaf]
    end

    type t =
      { role : Role.t
      ; content : Content.t
      }
    [@@deriving sexp_of, jsonaf_of]
  end

  module Result : sig
    type t =
      { description : string option
      ; messages : Message.t list
      }
    [@@deriving sexp_of, jsonaf_of]
  end
end
