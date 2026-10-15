open! Core
open Jsonaf.Export
open Mcp_protocol

module Empty = struct
  type t = unit

  let jsonaf_of_t () = Json_object.jsonaf_of_t Jsonaf.jsonaf_of_t String.Map.empty
end

module List_params = struct
  type t = { cursor : string option [@jsonaf.option] }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Tool_call = struct
  type t =
    { name : string
    ; arguments : Jsonaf.t Json_object.t [@default String.Map.empty]
    ; task : Jsonaf.t option [@jsonaf.option]
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Resource_read = struct
  type t = { uri : string } [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Prompt_get = struct
  type t =
    { name : string
    ; arguments : string Json_object.t [@default String.Map.empty]
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Cancelled = struct
  type t = { request_id : Request.Id.t [@key "requestId"] }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Initialize = struct
  type t =
    { protocol_version : string [@key "protocolVersion"]
    ; capabilities : Jsonaf.t Json_object.t
    ; client_info : Implementation.t [@key "clientInfo"]
    }
  [@@deriving of_jsonaf] [@@jsonaf.allow_extra_fields]
end

module Capabilities = struct
  type t =
    { tools : Empty.t option [@jsonaf.option]
    ; resources : Empty.t option [@jsonaf.option]
    ; prompts : Empty.t option [@jsonaf.option]
    }
  [@@deriving jsonaf_of]
end

module Initialized = struct
  type t =
    { protocol_version : string [@key "protocolVersion"]
    ; capabilities : Capabilities.t
    ; server_info : Implementation.t [@key "serverInfo"]
    ; instructions : string option [@jsonaf.option]
    }
  [@@deriving jsonaf_of]
end

module Tools = struct
  type t = { tools : Tool.t list } [@@deriving jsonaf_of]
end

module Resources = struct
  type t = { resources : Resource.t list } [@@deriving jsonaf_of]
end

module Resource_templates = struct
  type t = { resource_templates : Empty.t list [@key "resourceTemplates"] }
  [@@deriving jsonaf_of]
end

module Contents = struct
  type t = { contents : Resource.Contents.t list } [@@deriving jsonaf_of]
end

module Prompts = struct
  type t = { prompts : Prompt.t list } [@@deriving jsonaf_of]
end
