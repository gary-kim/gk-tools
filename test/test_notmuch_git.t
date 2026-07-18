Integration test against the real notmuch-git (the dune cram stanza only
enables this test when notmuch-git is on PATH).

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export XDG_CONFIG_HOME="$HOME/.config"
  $ export XDG_DATA_HOME="$HOME/.local/share"
  $ mkdir -p "$XDG_CONFIG_HOME"
  $ export GIT_CONFIG_SYSTEM=/dev/null
  $ export GIT_CONFIG_GLOBAL="$PWD/gitconfig"
  $ cat > "$GIT_CONFIG_GLOBAL" << 'EOF'
  > [user]
  > 	name = test
  > 	email = test@example.com
  > [init]
  > 	defaultBranch = master
  > EOF
  $ strip_log() { sed -E 's/^[0-9-]+ [0-9:.+Z-]+ //'; }

A tiny notmuch database: two messages, one tagged [work]. safe_fraction is
raised because any tag change in a two-message database is a "large
fraction"; real installs keep the default safety check.

  $ export NOTMUCH_CONFIG="$PWD/notmuch-config"
  $ mkdir -p mail
  $ cat > "$NOTMUCH_CONFIG" << EOF
  > [database]
  > path=$PWD/mail
  > [user]
  > name=test
  > primary_email=test@example.com
  > [new]
  > tags=inbox
  > [git]
  > safe_fraction=1
  > EOF
  $ mkdir -p mail/new mail/cur mail/tmp
  $ cat > mail/new/one << 'EOF'
  > From: alice@example.com
  > To: test@example.com
  > Subject: one
  > Date: Thu, 01 Jan 2026 00:00:00 +0000
  > Message-ID: <msg-one@example.com>
  > 
  > body one
  > EOF
  $ cat > mail/new/two << 'EOF'
  > From: bob@example.com
  > To: test@example.com
  > Subject: two
  > Date: Thu, 01 Jan 2026 00:00:01 +0000
  > Message-ID: <msg-two@example.com>
  > 
  > body two
  > EOF
  $ notmuch new --quiet
  $ notmuch tag +work -- id:msg-one@example.com

setup refuses to run without configuration.

  $ gkt notmuch setup 2>&1
  "notmuch-git is not configured: set [notmuch_git remote]"
  [1]

Seed a shared tag repo from this box's current state, standing in for the
"old" machine.

  $ git init -q --bare origin.git
  $ NOTMUCH_GIT_DIR="$PWD/seed-repo" notmuch-git -l error init > /dev/null
  $ NOTMUCH_GIT_DIR="$PWD/seed-repo" notmuch-git -l error commit seed
  $ git --git-dir="$PWD/seed-repo" remote add origin "$PWD/origin.git"
  $ git --git-dir="$PWD/seed-repo" push -q -u origin master

On the "new box": drop the work tag locally, then setup clones the remote and
force-checkouts the tags back into the database.

  $ export NOTMUCH_GIT_DIR="$PWD/box-repo"
  $ notmuch tag -work -- id:msg-one@example.com
  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << EOF
  > ((notmuch_git ((remote $PWD/origin.git))))
  > EOF
  $ gkt notmuch setup 2>&1 | strip_log
  Info (cloned(remote $TESTCASE_ROOT/origin.git))
  Info "checked out tags into the notmuch database"

  $ notmuch search --output=tags -- id:msg-one@example.com
  inbox
  unread
  work

sync commits, pulls, then pushes: a new local tag ends up on the remote with
the default commit message.

  $ notmuch tag +urgent -- id:msg-two@example.com
  $ gkt notmuch sync 2>&1 | strip_log
  Info committed
  Info pulled
  Info pushed

  $ git --git-dir=origin.git log -1 --format=%s master
  gkt: auto notmuch sync

sync also applies remote changes to the local database: another clone adds a
tag with plain git (the tag repo is just trees of empty files), and after
sync the local database has it.

  $ git clone -q origin.git remote-change
  $ tagdir=$(dirname "$(find remote-change -type f -name work)")
  $ touch "$tagdir/flagged"
  $ git -C remote-change add --all
  $ git -C remote-change commit -q -m remote-change
  $ git -C remote-change push -q origin master
  $ notmuch search --output=tags -- id:msg-one@example.com
  inbox
  unread
  work

  $ gkt notmuch sync -m 'sync from box' 2>&1 | strip_log
  Info committed
  Info pulled
  Info pushed

  $ notmuch search --output=tags -- id:msg-one@example.com
  flagged
  inbox
  unread
  work

  $ git --git-dir=origin.git log -1 --format=%s master
  remote-change
