Set up an isolated environment with a fake rbw command and a temporary HOME.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$HOME" "$PWD/bin"

Fake rbw that supports search, get --raw, and edit. The edit handler mimics
real rbw's behavior when stdin is not a TTY (see rbw/src/edit.rs): instead of
opening $EDITOR, rbw reads the new content from stdin.

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     # Args: --folder SECRET_FILES <term>
  >     echo "SECRET_FILES/upload-target"
  >     ;;
  >   get)
  >     # Args: --raw --folder SECRET_FILES RECORD
  >     printf '{"id":"id-1","folder":"SECRET_FILES","name":"upload-target","data":null,"fields":[{"name":"filepath","value":"target.conf","type":"text"}],"notes":"old contents\\n","history":[]}\n'
  >     ;;
  >   edit)
  >     # Args: --folder SECRET_FILES RECORD
  >     # Real rbw reads from stdin when not a TTY.
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

Pre-populate the local file with new content we want to upload.

  $ printf 'fresh local content\nline two\n' > "$HOME/target.conf"

Upload runs the diff (record vs local) on stdout, then writes local bytes to
the record. We confirm via the side log that the shim wrote what we expected.

  $ gkt rbw files upload -yes upload-target 2>/dev/null
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
