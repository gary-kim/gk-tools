Set up an isolated environment with a fake rbw command and a temporary HOME.

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

Check exits non-zero when any local file differs from its Bitwarden record.
stdout shows the diff for diff-case and the raw contents for new-case;
the missing-filepath record causes the iteration to error out.

  $ gkt rbw files check 2>/dev/null
  @|-1,1 +1,1 ============================================================
  -|stale contents
  +|new version
  brand new file
  [1]

Local files should not have been modified.

  $ cat "$HOME/match.conf"
  matching contents
  $ cat "$HOME/diff.conf"
  stale contents
  $ test ! -e "$HOME/new.conf"

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
