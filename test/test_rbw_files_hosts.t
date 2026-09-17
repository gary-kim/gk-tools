Set up an isolated environment with a fake rbw command and a temporary HOME.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export PATH="$PWD/bin:$PATH"
  $ mkdir -p "$HOME" "$PWD/bin"

Fake rbw with records exercising hosts_regex scoping. bad-regex sorts last in
search order so bulk runs show skip/write behavior before erroring on it;
other-nofilepath has a hosts_regex matching neither test hostname and no
filepath, proving the host gate runs before resolution.

  $ cat > bin/rbw << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > cmd="$1"; shift
  > case "$cmd" in
  >   sync) exit 0 ;;
  >   search)
  >     echo "SECRET_FILES/all-hosts"
  >     echo "SECRET_FILES/this-host"
  >     echo "SECRET_FILES/other-host"
  >     echo "SECRET_FILES/other-nofilepath"
  >     echo "SECRET_FILES/negated"
  >     echo "SECRET_FILES/bad-regex"
  >     ;;
  >   get)
  >     record="${@: -1}"
  >     case "$record" in
  >       all-hosts)
  >         printf '{"id":"id-1","folder":"SECRET_FILES","name":"all-hosts","data":null,"fields":[{"name":"filepath","value":"all.conf","type":"text"}],"notes":"for all\\n","history":[]}\n'
  >         ;;
  >       this-host)
  >         printf '{"id":"id-2","folder":"SECRET_FILES","name":"this-host","data":null,"fields":[{"name":"filepath","value":"this.conf","type":"text"},{"name":"hosts_regex","value":"gamma\\\\.garykim\\\\.dev","type":"text"}],"notes":"only gamma\\n","history":[]}\n'
  >         ;;
  >       other-host)
  >         printf '{"id":"id-3","folder":"SECRET_FILES","name":"other-host","data":null,"fields":[{"name":"filepath","value":"other.conf","type":"text"},{"name":"hosts_regex","value":"delta.*","type":"text"}],"notes":"only delta\\n","history":[]}\n'
  >         ;;
  >       other-nofilepath)
  >         printf '{"id":"id-4","folder":"SECRET_FILES","name":"other-nofilepath","data":null,"fields":[{"name":"hosts_regex","value":"nowhere\\\\.invalid","type":"text"}],"notes":"unused\\n","history":[]}\n'
  >         ;;
  >       negated)
  >         printf '{"id":"id-5","folder":"SECRET_FILES","name":"negated","data":null,"fields":[{"name":"filepath","value":"negated.conf","type":"text"},{"name":"hosts_regex","value":"!gamma.*","type":"text"}],"notes":"not gamma\\n","history":[]}\n'
  >         ;;
  >       bad-regex)
  >         printf '{"id":"id-6","folder":"SECRET_FILES","name":"bad-regex","data":null,"fields":[{"name":"filepath","value":"bad.conf","type":"text"},{"name":"hosts_regex","value":"[","type":"text"}],"notes":"unused\\n","history":[]}\n'
  >         ;;
  >     esac
  >     ;;
  > esac
  > EOF
  $ chmod +x bin/rbw

Bulk apply writes matching records, skips host mismatches with a log line, and
errors on the invalid pattern (which sorts last).

  $ gkt rbw files apply -hostname gamma.garykim.dev |& sed -E 's/^[0-9-]+ [0-9:.+Z-]+ //; s|'"$HOME"'|$HOME|g'
  Info (wrote(target $HOME/all.conf)(perm 0o600))
  Info (wrote(target $HOME/this.conf)(perm 0o600))
  Info ("skipping (host mismatch)"(name other-host)(hostname gamma.garykim.dev)(pattern delta.*))
  Info ("skipping (host mismatch)"(name other-nofilepath)(hostname gamma.garykim.dev)(pattern"nowhere\\.invalid"))
  Info ("skipping (host mismatch)"(name negated)(hostname gamma.garykim.dev)(pattern !gamma.*))
  (("invalid hosts_regex" (name bad-regex)) ("invalid pattern" (pattern [)))
  [1]

  $ cat "$HOME/all.conf"
  for all
  $ cat "$HOME/this.conf"
  only gamma
  $ [[ ! -e "$HOME/other.conf" ]]
  $ [[ ! -e "$HOME/negated.conf" ]]
  $ [[ ! -e "$HOME/bad.conf" ]]

Applying an explicitly named record errors when the host does not match.

  $ gkt rbw files apply other-host -hostname gamma.garykim.dev 2>&1
  ("record does not apply to this host" (name other-host) (pattern delta.*)
   (hostname gamma.garykim.dev))
  [1]
  $ [[ ! -e "$HOME/other.conf" ]]

A negated pattern applies on hosts outside the excluded set.

  $ gkt rbw files apply negated -hostname delta.garykim.dev 2>/dev/null
  $ cat "$HOME/negated.conf"
  not gamma

Upload refuses records that do not apply to this host, before any diff.

  $ gkt rbw files upload -yes other-host -hostname gamma.garykim.dev 2>&1
  ("record does not apply to this host" (name other-host) (pattern delta.*)
   (hostname gamma.garykim.dev))
  [1]

Cat never filters by host.

  $ gkt rbw files cat other-host
  only delta

Verify-records validates every record's metadata regardless of host scoping,
collecting all errors.

  $ gkt rbw files verify-records 2>&1
  ((("invalid record" (name other-nofilepath))
    ("record missing filepath field" (name other-nofilepath)))
   (("invalid record" (name bad-regex))
    (("invalid hosts_regex" (name bad-regex)) ("invalid pattern" (pattern [)))))
  [1]
