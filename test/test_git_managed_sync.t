Set up an isolated git environment with commit signing enabled via [/bin/false]
to verify managed-sync bypasses signing.

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

managed-sync uses [-c] overrides rather than writing to the repo-local config.

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

Without arguments or configured repos, the command errors.

  $ gkt git managed-sync -yes 2>&1
  "no repos to sync: pass -id/-dir or set [git_managed_sync repos] in the config"
  [1]

With no arguments, configured repos are synced in order; [repo-b] rebases onto
[repo-a]'s push.

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

An empty [git_managed_sync] section loads, but running with no arguments
errors.

  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << 'EOF'
  > ((git_managed_sync ()))
  > EOF
  $ gkt git managed-sync -yes 2>&1
  "no repos to sync: pass -id/-dir or set [git_managed_sync repos] in the config"
  [1]

An unknown id is an error.

  $ gkt git managed-sync -yes -id repo-a 2>&1
  ("unknown repo id" (id repo-a))
  [1]

Without -yes, a non-tty stdin refuses the push after showing the diff, keeping
the local commit.

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

A repo left mid-rebase by a conflicting pull is not committed over on the next
run.

  $ git clone -q origin.git side-a 2>/dev/null
  $ (cd side-a && echo base > shared && git add shared && git commit -q --no-gpg-sign -m add-shared && git push -q)
  $ git clone -q origin.git side-b 2>/dev/null
  $ (cd side-a && echo aaa > shared && git commit -q --no-gpg-sign -am change-a && git push -q)
  $ (cd side-b && echo bbb > shared && git commit -q --no-gpg-sign -am change-b)

The first sync hits a rebase conflict, leaving the repo mid-rebase.

  $ gkt git managed-sync -yes -dir side-b > /dev/null 2>&1; echo "exit: $?"
  exit: 1
  $ git -C side-b status --porcelain
  UU shared

The next run refuses to sync while conflicts remain unresolved.

  $ gkt git managed-sync -yes -dir side-b 2>&1 | strip_log
  Info (syncing(dir side-b))
  (("sync failed" (dir side-b))
   ("refusing to sync: unresolved merge conflicts" (dir side-b)
    (paths (shared))))
  [1]
  $ git --git-dir=origin.git log -1 --format=%s master
  change-a

Staging the resolution without [git rebase --continue] leaves the repo
mid-rebase, so sync still refuses.

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
