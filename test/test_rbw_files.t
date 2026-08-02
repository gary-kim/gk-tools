Set up an isolated environment with a fake rbw command and a temporary HOME.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$HOME" "$PWD/bin"

Fake rbw that emits JSON for SECRET_FILES records. printf is used for the
JSON to keep escaping sane.

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     # Args: --folder SECRET_FILES <term>
  >     echo "SECRET_FILES/match-case"
  >     echo "SECRET_FILES/diff-case"
  >     echo "SECRET_FILES/new-case"
  >     echo "SECRET_FILES/missing-path"
  >     ;;
  >   get)
  >     # Args: --raw --folder SECRET_FILES RECORD
  >     record="${@: -1}"
  >     case "$record" in
  >       match-case)
  >         printf '{"id":"id-1","folder":"SECRET_FILES","name":"match-case","data":null,"fields":[{"name":"filepath","value":"match.conf","type":"text"}],"notes":"matching contents\\n","history":[]}\n'
  >         ;;
  >       diff-case)
  >         printf '{"id":"id-2","folder":"SECRET_FILES","name":"diff-case","data":null,"fields":[{"name":"filepath","value":"diff.conf","type":"text"}],"notes":"new version\\n","history":[]}\n'
  >         ;;
  >       new-case)
  >         printf '{"id":"id-3","folder":"SECRET_FILES","name":"new-case","data":null,"fields":[{"name":"filepath","value":"new.conf","type":"text"}],"notes":"brand new file\\n","history":[]}\n'
  >         ;;
  >       missing-path)
  >         printf '{"id":"id-4","folder":"SECRET_FILES","name":"missing-path","data":null,"fields":[],"notes":"unused\\n","history":[]}\n'
  >         ;;
  >     esac
  >     ;;
  > esac
  > EOF
  $ chmod +x bin/rbw

Pre-populate local files for the matching and diff cases.

  $ echo "matching contents" > "$HOME/match.conf"
  $ echo "stale contents" > "$HOME/diff.conf"
  $ chmod 644 "$HOME/match.conf"

Cat prints Bitwarden contents for a record.

  $ gkt rbw files cat diff-case
  new version

Cat resolves the id before printing contents.

  $ gkt rbw files cat bogus 2>&1
  ("no record matching id" (id bogus))
  [1]

TODO: add coverage for the ambiguous-id branch of [find_record_name_in_list]
(e.g. two records whose names are case-insensitively equal).

Check exits non-zero when any local file differs from its Bitwarden record.
Per-file findings come through async_log on stderr; the missing-filepath
record causes the iteration to error out.

  $ gkt rbw files check |& sed -E 's/^[0-9-]+ [0-9:.+Z-]+ //; s|'"$HOME"'|$HOME|g'
  Info ("file matches"(target $HOME/match.conf))
  ------ $HOME/diff.conf
  ++++++ bitwarden:diff-case
  @|-1,1 +1,1 ============================================================
  -|stale contents
  +|new version
  ------ $HOME/new.conf
  ++++++ bitwarden:new-case
  @|-1,0 +1,1 ============================================================
  +|brand new file
  Info ("file does not exist locally"(target $HOME/new.conf))
  ("errors during check" (mismatched (diff-case new-case))
   (errors (("record missing filepath field" (name missing-path)))))
  [1]

Local files should not have been modified.

  $ cat "$HOME/match.conf"
  matching contents
  $ cat "$HOME/diff.conf"
  stale contents
  $ [[ ! -e "$HOME/new.conf" ]]

Apply writes all preceding records with 0600 perms and then errors on the
missing-filepath record.

  $ gkt rbw files apply 2>/dev/null
  [1]
  $ cat "$HOME/match.conf"
  matching contents
  $ cat "$HOME/diff.conf"
  new version
  $ cat "$HOME/new.conf"
  brand new file
  $ stat -c "%a" "$HOME/new.conf"
  600
  $ stat -c "%a" "$HOME/diff.conf"
  600
  $ stat -c "%a" "$HOME/match.conf"
  644

Reset the local state so we can re-run apply with explicit ids.

  $ echo "matching contents" > "$HOME/match.conf"
  $ echo "stale contents" > "$HOME/diff.conf"
  $ rm -f "$HOME/new.conf"

Apply with explicit ids only touches those records and skips the rest.

  $ gkt rbw files apply diff-case new-case 2>/dev/null
  $ cat "$HOME/diff.conf"
  new version
  $ cat "$HOME/new.conf"
  brand new file
  $ cat "$HOME/match.conf"
  matching contents

Apply with an unknown id errors before writing anything.

  $ rm -f "$HOME/new.conf"
  $ echo "stale contents" > "$HOME/diff.conf"
  $ gkt rbw files apply diff-case bogus 2>&1 | tail -1
  ("no record matching id" (id bogus))
  [1]
  $ cat "$HOME/diff.conf"
  stale contents
  $ [[ ! -e "$HOME/new.conf" ]]
