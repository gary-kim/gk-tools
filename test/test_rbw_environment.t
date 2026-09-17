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

export single-quotes tricky values into a shell-eval-able line.

  $ eval "$(gkt rbw environment export BAR_BAZ)" && printf '%s\n' "$BAR_BAZ"
  tricky'value with spaces

exec runs a command with the requested env vars, passes post -- arguments
through untouched, and preserves the child's exit status.

  $ gkt rbw environment exec -environment FOO,BAR_BAZ -- sh -c 'printf "%s\n" "$FOO" "$BAR_BAZ"'
  foo-value
  tricky'value with spaces
  $ gkt rbw environment exec -environment FOO -- sh -c 'printf "%s\n" "$1"' sh -n
  -n
  $ gkt rbw environment exec -environment BAR_BAZ -- sh -c 'exit 7' 2>&1
  [7]

Invalid env var names are rejected before rbw is called.

  $ rm -f "$RBW_LOG"
  $ gkt rbw environment exec -environment 1bad,has-dash -- true 2>&1
  (("invalid env var name" (name 1bad))
   ("invalid env var name" (name has-dash)))
  [1]
  $ test ! -s "$RBW_LOG" || cat "$RBW_LOG"

A sync failure is non-fatal; the cached vault still serves.

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) echo "rbw sync: no network" >&2; exit 1 ;;
  >   search) echo "ENV/ENV:FOO" ;;
  >   get) printf 'foo-value\n' ;;
  > esac
  > EOF

  $ gkt rbw environment get FOO 2>/dev/null
  foo-value
