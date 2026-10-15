Set up an isolated environment with a fake rbw command and a temporary HOME.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$HOME" "$PWD/bin"

Fake rbw supporting search, get --raw, and edit (reading new content from stdin
when not a TTY).

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     echo "SECRET_FILES/upload-target"
  >     ;;
  >   get)
  >     printf '{"id":"id-1","folder":"SECRET_FILES","name":"upload-target","data":null,"fields":[{"name":"filepath","value":"target.conf","type":"text"}],"notes":"old contents\\n","history":[]}\n'
  >     ;;
  >   edit)
  >     contents=$(cat)
  >     {
  >       echo "rbw-edit-result-for ${@: -1}:"
  >       printf '%s' "$contents"
  >     } >> "$EDIT_LOG"
  >     ;;
  > esac
  > EOF
  $ chmod +x bin/rbw
  $ export EDIT_LOG="$PWD/edit.log"

Pre-populate the local file with new content to upload.

  $ printf 'fresh local content\nline two\n' > "$HOME/target.conf"

Upload prints the diff on stdout and writes the local file's contents to the
record.

  $ gkt rbw files upload -yes Upload-Target 2>/dev/null
  ------ bitwarden:upload-target
  ++++++ $TESTCASE_ROOT/home/target.conf
  @|-1,1 +1,2 ============================================================
  -|old contents
  +|fresh local content
  +|line two

  $ cat "$EDIT_LOG"
  rbw-edit-result-for upload-target:
  fresh local content
  line two

Without -yes, a non-interactive stdin/stdout is an error, not a hang.

  $ printf 'n\n' | timeout 10 gkt rbw files upload upload-target 2>&1
  ------ bitwarden:upload-target
  ++++++ $TESTCASE_ROOT/home/target.conf
  @|-1,1 +1,2 ============================================================
  -|old contents
  +|fresh local content
  +|line two
  "stdin/stdout is not a tty: pass -yes to skip the confirmation prompt"
  [1]
