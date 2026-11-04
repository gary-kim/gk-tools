open! Core
open! Async
open Jsonaf.Export

module Tool = struct
  type t = { name : string } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Request = struct
  type t =
    { input : Jsonaf.t list
    ; tools : Tool.t list [@default []]
    ; include_ : string list [@key "include"] [@default []]
    ; prompt_cache_key : string
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Response = struct
  type t =
    { status : string
    ; output : Jsonaf.t list
    }
  [@@deriving jsonaf_of]
end

module Function_call = struct
  type t =
    { type_ : string [@key "type"]
    ; call_id : string
    ; name : string
    ; arguments : string
    }
  [@@deriving jsonaf_of]
end

module Content = struct
  type t =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; filename : string option [@jsonaf.option]
    ; file_data : string option [@jsonaf.option]
    ; image_url : string option [@jsonaf.option]
    }
  [@@deriving jsonaf]
end

module Tool_output = struct
  type t =
    { type_ : string [@key "type"]
    ; call_id : string
    ; output : Content.t list
    }
  [@@deriving of_jsonaf]
end

module Attachment_arguments = struct
  type t = { part_id : int option } [@@deriving jsonaf_of]
end

module Lookup_arguments = struct
  type t = { key : int } [@@deriving of_jsonaf, sexp_of]
end

let with_responses responses ~f =
  List.map responses ~f:(fun respond ~body _ _ ->
    let%bind body = Cohttp_async.Body.to_string body in
    Jsonaf.of_string body
    |> [%of_jsonaf: Request.t]
    |> respond
    |> Cohttp_async.Server.respond_string)
  |> Test_server.with_handlers ~f
;;
