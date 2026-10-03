Set up an isolated gpg home with a passphrase-less signing key and a fake
ansible-playbook first on PATH. The fake prints its arguments and the basename
of $ANSIBLE_CONFIG, with the real hostname normalized out, and fails only when
FAKE_PLAYBOOK_FAIL is set.

  $ set -o pipefail
  $ export GNUPGHOME=$(mktemp -d)
  $ gpg -q --batch --passphrase '' --quick-gen-key test@example.com ed25519 sign never
  $ gpg -q --batch --export test@example.com > trusted.gpg
  $ host=$(uname -n); short=${host%%.*}
  $ mkdir bin
  $ cat > bin/ansible-playbook << EOF
  > #!/bin/sh
  > echo "ansible-playbook \$* \$(basename "\$ANSIBLE_CONFIG")" | sed "s/,$host,$short,/,HOST,SHORT,/"
  > [ -z "\${FAKE_PLAYBOOK_FAIL:-}" ]
  > EOF
  $ chmod +x bin/ansible-playbook
  $ export PATH="$PWD/bin:$PATH"
  $ strip_log() { sed -E 's/^[0-9-]+ [0-9:.+Z-]+ //; s|'"$PWD"'|$PWD|g'; }

publish builds a tarball for the given VERSION under the gk-workstation-ansible/
prefix and signs it into serve/. The keyring is a bare relative name, which the
tool must make absolute before gpgv sees it.

  $ publish() {
  >   rm -rf src serve && mkdir -p src/gk-workstation-ansible serve
  >   echo "$1" > src/gk-workstation-ansible/VERSION
  >   touch src/gk-workstation-ansible/ansible.cfg src/gk-workstation-ansible/inventory.yaml src/gk-workstation-ansible/playbook.yaml
  >   tar -czf serve/gk-workstation-ansible.tar.gz -C src gk-workstation-ansible
  >   gpg -q --batch --detach-sign -o serve/gk-workstation-ansible.tar.gz.sig serve/gk-workstation-ansible.tar.gz
  > }
  $ cat > config.sexp << EOF
  > ((url file://$PWD/serve/gk-workstation-ansible.tar.gz)
  >  (keyring trusted.gpg)
  >  (state_dir state))
  > EOF

The first run applies the tarball and records it as current.

  $ publish 100
  $ gkt workstation-ansible pull -config config.sexp 2>&1 | strip_log
  Info (fetched(url file://$PWD/serve/gk-workstation-ansible.tar.gz))
  Info verified
  Info (applying(version 100))
  ansible-playbook -c local -l localhost,HOST,SHORT,127.0.0.1 -i inventory.yaml playbook.yaml ansible.cfg
  Info (applied(version 100))
  $ cat state/current/VERSION
  100

The same tarball again is already applied: no playbook run, exit 0.

  $ gkt workstation-ansible pull -config config.sexp 2>&1 | strip_log
  Info (fetched(url file://$PWD/serve/gk-workstation-ansible.tar.gz))
  Info verified
  Info ("already applied"(version 100))

A lower version, correctly signed, is refused and current is unchanged.

  $ publish 50
  $ gkt workstation-ansible pull -config config.sexp 2>&1 | strip_log
  Info (fetched(url file://$PWD/serve/gk-workstation-ansible.tar.gz))
  Info verified
  ("refusing to roll back" (applied 100) (version 50))
  [1]
  $ cat state/current/VERSION
  100

A higher version with a bad signature is refused before anything is extracted.

  $ publish 200
  $ echo corrupt >> serve/gk-workstation-ansible.tar.gz
  $ gkt workstation-ansible pull -config config.sexp 2>&1 | grep -o 'BAD signature'
  BAD signature
  [1]
  $ ls state/incoming
  gk-workstation-ansible.tar.gz
  gk-workstation-ansible.tar.gz.sig
  $ cat state/current/VERSION
  100

A playbook failure leaves current unchanged and does not record the new tree.

  $ publish 200
  $ FAKE_PLAYBOOK_FAIL=1 gkt workstation-ansible pull -config config.sexp > /dev/null 2>&1; echo "exit: $?"
  exit: 1
  $ cat state/current/VERSION
  100
  $ ls state
  100
  current
  incoming
  lock

The same tarball with the playbook succeeding is applied, and the old version
directory is pruned.

  $ gkt workstation-ansible pull -config config.sexp 2>&1 | strip_log
  Info (fetched(url file://$PWD/serve/gk-workstation-ansible.tar.gz))
  Info verified
  Info (applying(version 200))
  ansible-playbook -c local -l localhost,HOST,SHORT,127.0.0.1 -i inventory.yaml playbook.yaml ansible.cfg
  Info (applied(version 200))
  $ cat state/current/VERSION
  200
  $ ls state
  200
  current
  incoming
  lock

  $ gpgconf --kill all
  $ rm -rf "$GNUPGHOME"
