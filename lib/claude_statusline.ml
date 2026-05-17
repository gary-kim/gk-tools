open! Core
open! Async
open Deferred.Or_error.Let_syntax
open Jsonaf.Export

module Input = struct
  module Model = struct
    type t = { display_name : string option [@default None] }
    [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
  end

  module Cost = struct
    type t = { total_cost_usd : float option [@default None] }
    [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
  end

  module Current_usage = struct
    type t =
      { input_tokens : int option [@default None]
      ; cache_read_input_tokens : int option [@default None]
      ; cache_creation_input_tokens : int option [@default None]
      ; output_tokens : int option [@default None]
      }
    [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
  end

  module Context_window = struct
    type t =
      { context_window_size : int option [@default None]
      ; current_usage : Current_usage.t option [@default None]
      ; total_input_tokens : int option [@default None]
      ; total_output_tokens : int option [@default None]
      }
    [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
  end

  type t =
    { model : Model.t option [@default None]
    ; cost : Cost.t option [@default None]
    ; context_window : Context_window.t option [@default None]
    }
  [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
end

let format_optional_int = Option.value_map ~default:"" ~f:Int.to_string
let format_cost cost = sprintf "%.3f" cost

let context_segment (context_window : Input.Context_window.t option) =
  match context_window with
  | None -> None
  | Some { context_window_size = None; _ } -> None
  | Some { current_usage = None; _ } -> None
  | Some
      { context_window_size = Some context_size
      ; current_usage = Some usage
      ; total_input_tokens
      ; total_output_tokens
      } ->
    (match usage.input_tokens with
     | None -> None
     | Some input_tokens ->
       let cache_read = Option.value usage.cache_read_input_tokens ~default:0 in
       let cache_write = Option.value usage.cache_creation_input_tokens ~default:0 in
       let used = input_tokens + cache_read + cache_write in
       let used = Int.to_string used in
       let context_size = Int.to_string context_size in
       let input_tokens = Int.to_string input_tokens in
       let output_tokens = format_optional_int usage.output_tokens in
       let cache_read = format_optional_int usage.cache_read_input_tokens in
       let cache_write = format_optional_int usage.cache_creation_input_tokens in
       let total_input = format_optional_int total_input_tokens in
       let total_output = format_optional_int total_output_tokens in
       Some
         [%string
           "ctx: %{used} / %{context_size} | in: %{input_tokens} out: %{output_tokens} \
            cache_r: %{cache_read} cache_w: %{cache_write} total_in: %{total_input} \
            total_out: %{total_output}"])
;;

let render raw =
  let open Or_error.Let_syntax in
  let%map input =
    Or_error.try_with (fun () -> Jsonaf.of_string raw |> Input.t_of_jsonaf)
    |> Or_error.tag ~tag:"failed to parse Claude statusline JSON"
  in
  let model = Option.bind input.model ~f:(fun model -> model.display_name) in
  let cost =
    Option.bind input.cost ~f:(fun cost -> cost.total_cost_usd)
    |> Option.map ~f:(fun cost -> [%string "$%{format_cost cost}"])
  in
  [ model; cost; context_segment input.context_window ]
  |> List.filter_opt
  |> String.concat ~sep:" | "
;;

let command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Render a Claude Code statusline from stdin JSON"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind raw =
         Deferred.Or_error.try_with ~extract_exn:true (fun () ->
           Reader.contents (Lazy.force Reader.stdin))
       in
       let%map line = render raw |> Deferred.return in
       print_endline line)
;;
