Set up an isolated environment with a fake rbw command and a temporary HOME.

  $ export HOME="$PWD/home"
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$HOME" "$PWD/bin"

Fake rbw that emits canned responses for SECRET_FILES records.

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     echo "SECRET_FILES/match-case"
  >     echo "SECRET_FILES/diff-case"
  >     echo "SECRET_FILES/new-case"
  >     echo "SECRET_FILES/missing-path"
  >     ;;
  >   get)
  >     # Args: --folder SECRET_FILES [--field FIELD] RECORD
  >     if [ "${3:-}" = "--field" ]; then
  >       record="$5"
  >       case "$record" in
  >         match-case)   echo "match.conf" ;;
  >         diff-case)    echo "diff.conf" ;;
  >         new-case)     echo "new.conf" ;;
  >         missing-path) exit 1 ;;
  >       esac
  >     else
  >       record="$3"
  >       case "$record" in
  >         match-case)   echo "matching contents" ;;
  >         diff-case)    echo "new version" ;;
  >         new-case)     echo "brand new file" ;;
  >         missing-path) echo "unused" ;;
  >       esac
  >     fi
  >     ;;
  > esac
  > EOF
  $ chmod +x bin/rbw

Pre-populate local files for the matching and diff cases.

  $ echo "matching contents" > "$HOME/match.conf"
  $ echo "stale contents" > "$HOME/diff.conf"

Dry-run (default). stderr has log output with timestamps; drop it. stdout
should show the diff for diff-case and the raw contents for new-case.

  $ gk-tools rbw-place-secret-files 2>/dev/null
  @|-1,1 +1,1 ============================================================
  -|stale contents
  +|new version
  brand new file

Local files should not have been modified.

  $ cat "$HOME/match.conf"
  matching contents
  $ cat "$HOME/diff.conf"
  stale contents
  $ test ! -e "$HOME/new.conf"

Apply mode writes all files with 0600 perms and skips missing-path.

  $ gk-tools rbw-place-secret-files -apply 2>/dev/null
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
