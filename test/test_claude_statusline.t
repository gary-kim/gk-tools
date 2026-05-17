The Claude statusline command reads JSON on stdin and writes one status line.

  $ gkt misc claude statusline << 'EOF'
  > {"model":{"display_name":"Opus"},
  >  "cost":{"total_cost_usd":1.23456},
  >  "context_window":{
  >    "context_window_size":100000,
  >    "current_usage":{
  >      "input_tokens":1200,
  >      "cache_read_input_tokens":300,
  >      "cache_creation_input_tokens":50,
  >      "output_tokens":25
  >    },
  >    "total_input_tokens":2000,
  >    "total_output_tokens":100
  >  }}
  > EOF
  Opus | $1.235 | ctx: 1550 / 100000 | in: 1200 out: 25 cache_r: 300 cache_w: 50 total_in: 2000 total_out: 100
