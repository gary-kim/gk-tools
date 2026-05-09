Set up an isolated environment with a fake rbw command on PATH.

  $ set -o pipefail
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$PWD/bin"
  $ export RBW_LOG="$PWD/rbw.log"

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > printf '%s\n' "rbw $*" >> "$RBW_LOG"
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     echo "ENV/ENV:FOO"
  >     echo "ENV/ENV:BAR_BAZ"
  >     ;;
  >   get)
  >     record="${@: -1}"
  >     case "$record" in
  >       ENV:FOO)      printf 'foo-value\n' ;;
  >       ENV:BAR_BAZ)  printf "tricky'value with spaces\n" ;;
  >       *)            echo "unknown record: $record" >&2; exit 1 ;;
  >     esac
  >     ;;
  > esac
  > EOF
  $ chmod +x bin/rbw

list prints var names with the ENV: prefix stripped.

  $ gkt rbw environment list
  FOO
  BAR_BAZ

get prints just the value.

  $ gkt rbw environment get FOO
  foo-value

export prints a shell-eval-able line, single-quoting tricky values correctly.

  $ gkt rbw environment export FOO
  export FOO=foo-value
  $ gkt rbw environment export BAR_BAZ
  export BAR_BAZ='tricky'\''value with spaces'
  $ eval "$(gkt rbw environment export BAR_BAZ)" && printf '%s\n' "$BAR_BAZ"
  tricky'value with spaces

Invalid env var names are rejected before rbw is called.

  $ rm -f "$RBW_LOG"
  $ gkt rbw environment get 1bad 2>&1 | tail -1
  ("invalid env var name" (name 1bad))
  [1]
  $ gkt rbw environment export 'has space' 2>&1 | tail -1
  ("invalid env var name" (name "has space"))
  [1]
  $ test ! -s "$RBW_LOG" || cat "$RBW_LOG"
