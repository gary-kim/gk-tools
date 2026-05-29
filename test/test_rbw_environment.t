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

exec runs a command with the requested comma-separated env vars.

  $ gkt rbw environment exec -environment FOO,BAR_BAZ -- sh -c 'printf "%s\n" "$FOO" "$BAR_BAZ"'
  foo-value
  tricky'value with spaces

exec passes arguments after -- through to the command.

  $ gkt rbw environment exec -environment FOO -- sh -c 'printf "%s\n" "$1"' sh -n
  -n

exec preserves the child's non-zero exit status, without leaking secrets.

  $ gkt rbw environment exec -environment BAR_BAZ -- sh -c 'exit 7' 2>&1
  [7]

exec surfaces the underlying spawn error.

  $ gkt rbw environment exec -environment BAR_BAZ -- definitely-not-a-command 2>&1
  (Core_unix.fork_exec (exec definitely-not-a-command) ENOENT)
  [1]

Invalid env var names are rejected before rbw is called.

  $ rm -f "$RBW_LOG"
  $ gkt rbw environment get 1bad 2>&1 | tail -1
  ("invalid env var name" (name 1bad))
  [1]
  $ gkt rbw environment export 'has space' 2>&1 | tail -1
  ("invalid env var name" (name "has space"))
  [1]
  $ gkt rbw environment exec -environment FOO,1bad -- true 2>&1 | tail -1
  ("invalid env var name" (name 1bad))
  [1]
  $ gkt rbw environment exec -environment 1bad,has-dash -- true 2>&1
  (("invalid env var name" (name 1bad))
   ("invalid env var name" (name has-dash)))
  [1]
  $ gkt rbw environment exec -environment FOO,FOO -- true 2>&1 | tail -1
  "duplicate env var name"
  [1]
  $ test ! -s "$RBW_LOG" || cat "$RBW_LOG"

exec requires a command after --.

  $ gkt rbw environment exec -environment FOO 2>&1
  "command missing after --"
  [1]
