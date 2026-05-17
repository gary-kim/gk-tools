open! Core

let print_rendered json =
  Gk_tools.Claude_statusline.render json |> Or_error.ok_exn |> print_endline
;;

let%expect_test "full statusline payload" =
  print_rendered
    {|
    {"model":{"display_name":"Opus"},
     "cost":{"total_cost_usd":1.23456},
     "context_window":{
       "context_window_size":100000,
       "current_usage":{
         "input_tokens":1200,
         "cache_read_input_tokens":300,
         "cache_creation_input_tokens":50,
         "output_tokens":25
       },
       "total_input_tokens":2000,
       "total_output_tokens":100
     }}
    |};
  [%expect
    {|
    Opus | $1.235 | ctx: 1550 / 100000 | in: 1200 out: 25 cache_r: 300 cache_w: 50 total_in: 2000 total_out: 100
    |}]
;;

let%expect_test "missing cost" =
  print_rendered
    {|
    {"model":{"display_name":"Sonnet"},
     "context_window":{
       "context_window_size":200000,
       "current_usage":{"input_tokens":10}
     }}
    |};
  [%expect
    {|
    Sonnet | ctx: 10 / 200000 | in: 10 out:  cache_r:  cache_w:  total_in:  total_out:
    |}]
;;

let%expect_test "missing context" =
  print_rendered
    {|
    {"model":{"display_name":"Haiku"},
     "cost":{"total_cost_usd":0.9999}}
    |};
  [%expect {| Haiku | $1.000 |}]
;;

let%expect_test "empty input renders nothing" =
  print_rendered {|{}|};
  [%expect {| |}]
;;
