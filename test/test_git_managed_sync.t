Set up an isolated git environment. The scratch global gitconfig deliberately
enables commit signing with a broken gpg program: any commit that does not
bypass signing fails, proving managed-sync commits unsigned.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export XDG_CONFIG_HOME="$HOME/.config"
  $ mkdir -p "$XDG_CONFIG_HOME"
  $ export GIT_CONFIG_SYSTEM=/dev/null
  $ export GIT_CONFIG_GLOBAL="$PWD/gitconfig"
  $ cat > "$GIT_CONFIG_GLOBAL" << 'EOF'
  > [user]
  > 	name = test
  > 	email = test@example.com
  > [init]
  > 	defaultBranch = master
  > [commit]
  > 	gpgsign = true
  > [gpg]
  > 	program = /bin/false
  > EOF
  $ strip_log() { sed -E 's/^[0-9-]+ [0-9:.+Z-]+ //; s|'"$HOME"'|$HOME|g'; }

Seed an origin with one commit and clone a working repo.

  $ git init -q --bare origin.git
  $ git clone -q origin.git work 2>/dev/null
  $ (cd work && git commit -q --allow-empty --no-gpg-sign -m init && git push -q -u origin master)

Plain git commit refuses to run under the signing gitconfig.

  $ (cd work && touch f && git add f && git commit -q -m nope) 2>&1 | grep -o 'failed to sign' | head -1
  failed to sign
  [128]

managed-sync commits the staged file unsigned, shows the outgoing diff, and
pushes.

  $ gkt git managed-sync -yes -dir work 2>&1 | strip_log
  Info (syncing(dir work))
  Info (committed(dir work)(message"gkt: auto managed sync"))
  diff --git a/f b/f
  new file mode 100644
  index 0000000..e69de29
  Info (pushed(dir work)(count 1))

  $ git --git-dir=origin.git log --format=%s master
  gkt: auto managed sync
  init

The tool never writes to the repo-local config; unsigned commits and rebased
pulls come entirely from the [-c] overrides on its own git invocations, so a
plain git commit here still trips the signing gitconfig.

  $ git -C work config --local commit.gpgsign
  [1]
  $ git -C work config --local pull.rebase
  [1]
  $ (cd work && git commit -q --allow-empty -m nope) 2>&1 | grep -o 'failed to sign' | head -1
  failed to sign
  [128]

Re-running is a no-op.

  $ gkt git managed-sync -yes -dir work 2>&1 | strip_log
  Info (syncing(dir work))
  Info ("nothing to commit"(dir work))
  Info ("nothing to push"(dir work))

A commit pushed from another clone is rebased under local changes: history
stays linear and the outgoing diff shows only the local commit.

  $ git clone -q origin.git other
  $ (cd other && echo hello > g && git add g && git commit -q --no-gpg-sign -m other-change && git push -q)
  $ echo world > work/h
  $ gkt git managed-sync -yes -dir work 2>&1 | strip_log
  Info (syncing(dir work))
  Info (committed(dir work)(message"gkt: auto managed sync"))
  diff --git a/h b/h
  new file mode 100644
  index 0000000..cc628cc
  --- /dev/null
  +++ b/h
  @@ -0,0 +1 @@
  +world
  Info (pushed(dir work)(count 1))

  $ git --git-dir=origin.git log --format=%s master
  gkt: auto managed sync
  other-change
  gkt: auto managed sync
  init

Without arguments and without configuration, the command refuses to guess.

  $ gkt git managed-sync -yes 2>&1
  "no repos to sync: pass -id/-dir or set [git_managed_sync repos] in the config"
  [1]

Repos listed in the config sexp are synced in order when no DIR is given;
~/ expands against $HOME. repo-b is behind origin after repo-a pushes, so its
sync exercises the rebase path too.

  $ git clone -q origin.git "$HOME/repo-a"
  $ git clone -q origin.git "$HOME/repo-b"
  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << 'EOF'
  > ((git_managed_sync ((repos (((id repo-a) (dir ~/repo-a)) ((id repo-b) (dir ~/repo-b)))))))
  > EOF
  $ touch "$HOME/repo-a/a" "$HOME/repo-b/b"
  $ gkt git managed-sync -yes -m checkpoint 2>&1 | strip_log
  Info (syncing(dir $HOME/repo-a))
  Info (committed(dir $HOME/repo-a)(message checkpoint))
  diff --git a/a b/a
  new file mode 100644
  index 0000000..e69de29
  Info (pushed(dir $HOME/repo-a)(count 1))
  Info (syncing(dir $HOME/repo-b))
  Info (committed(dir $HOME/repo-b)(message checkpoint))
  diff --git a/b b/b
  new file mode 100644
  index 0000000..e69de29
  Info (pushed(dir $HOME/repo-b)(count 1))

  $ git --git-dir=origin.git log -3 --format=%s master
  checkpoint
  checkpoint
  gkt: auto managed sync

A single repo can be selected by its config id.

  $ echo more >> "$HOME/repo-a/a"
  $ gkt git managed-sync -yes -id repo-a 2>&1 | strip_log
  Info (syncing(dir $HOME/repo-a))
  Info (committed(dir $HOME/repo-a)(message"gkt: auto managed sync"))
  diff --git a/a b/a
  index e69de29..ef49dd8 100644
  --- a/a
  +++ b/a
  @@ -0,0 +1 @@
  +more
  Info (pushed(dir $HOME/repo-a)(count 1))

  $ git --git-dir=origin.git log -1 --format=%s master
  gkt: auto managed sync

An empty [git_managed_sync] section loads fine; only a run with no arguments
complains. Plain DIR arguments keep working without configured repos.

  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << 'EOF'
  > ((git_managed_sync ()))
  > EOF
  $ gkt git managed-sync -yes 2>&1
  "no repos to sync: pass -id/-dir or set [git_managed_sync repos] in the config"
  [1]

An id that is not in the config is an error, not a directory fallback.

  $ gkt git managed-sync -yes -id repo-a 2>&1
  ("unknown repo id" (id repo-a))
  [1]

Without -yes, a non-tty stdin refuses the push after showing the diff (piped
input is not accepted as confirmation); the local commit is kept.

  $ echo later > work/i
  $ printf 'n\n' | timeout 10 gkt git managed-sync -dir work 2>&1 | strip_log
  Info (syncing(dir work))
  Info (committed(dir work)(message"gkt: auto managed sync"))
  diff --git a/i b/i
  new file mode 100644
  index 0000000..e974158
  --- /dev/null
  +++ b/i
  @@ -0,0 +1 @@
  +later
  (("sync failed" (dir work))
   "stdin/stdout is not a tty: pass -yes to skip the confirmation prompt")
  [1]

  $ git --git-dir=origin.git log -1 --format=%s master
  gkt: auto managed sync
  $ git -C work log --format=%s -1
  gkt: auto managed sync

A repo left mid-rebase by a conflicting pull must not be silently committed
over on the next run. Seed a tracked file, then diverge two clones on it.

  $ git clone -q origin.git side-a 2>/dev/null
  $ (cd side-a && echo base > shared && git add shared && git commit -q --no-gpg-sign -m add-shared && git push -q)
  $ git clone -q origin.git side-b 2>/dev/null
  $ (cd side-a && echo aaa > shared && git commit -q --no-gpg-sign -am change-a && git push -q)
  $ (cd side-b && echo bbb > shared && git commit -q --no-gpg-sign -am change-b)

The first sync pulls and hits a rebase conflict, leaving the repo mid-rebase.

  $ gkt git managed-sync -yes -dir side-b > /dev/null 2>&1; echo "exit: $?"
  exit: 1
  $ git -C side-b status --porcelain
  UU shared

The next run refuses to commit over the unresolved conflict rather than staging
the markers, and origin is left untouched.

  $ gkt git managed-sync -yes -dir side-b 2>&1 | strip_log
  Info (syncing(dir side-b))
  (("sync failed" (dir side-b))
   ("refusing to sync: unresolved merge conflicts" (dir side-b)
    (paths (shared))))
  [1]
  $ git --git-dir=origin.git log -1 --format=%s master
  change-a

Staging the resolution without [git rebase --continue] leaves no unmerged
paths, but the repo is still mid-rebase on a detached HEAD; committing now
would strand the resolution there, so sync still refuses.

  $ (cd side-b && echo resolved > shared && git add shared)
  $ gkt git managed-sync -yes -dir side-b 2>&1 | strip_log
  Info (syncing(dir side-b))
  (("sync failed" (dir side-b))
   ("refusing to sync: rebase in progress" (dir side-b)))
  [1]

Finishing the rebase lets the next sync push the resolved commit.

  $ GIT_EDITOR=true git -C side-b -c commit.gpgsign=false rebase --continue > /dev/null 2>&1
  $ gkt git managed-sync -yes -dir side-b 2>&1 | strip_log
  Info (syncing(dir side-b))
  Info ("nothing to commit"(dir side-b))
  diff --git a/shared b/shared
  index 72943a1..2ab19ae 100644
  --- a/shared
  +++ b/shared
  @@ -1 +1 @@
  -aaa
  +resolved
  Info (pushed(dir side-b)(count 1))

  $ git --git-dir=origin.git log -2 --format=%s master
  change-b
  change-a
