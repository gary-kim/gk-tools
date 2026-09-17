Set up a fake sudo/dnf for mutations, snapshot fixtures standing in for the
libdnf5 reads, and a config in an isolated XDG_CONFIG_HOME. The fixture box:
core and fonts groups installed, a server-env environment requiring core, and
a mix of package reasons; mutating dnf invocations append to a log instead of
doing anything.

  $ set -o pipefail
  $ export HOME="$PWD/home"
  $ export XDG_CONFIG_HOME="$PWD/config"
  $ export PATH="$PWD/bin:$PATH"
  $ export DNF_LOG="$PWD/dnf.log"
  $ mkdir -p "$HOME" "$PWD/bin" "$XDG_CONFIG_HOME"

  $ cat > bin/dnf << 'EOF'
  > #!/usr/bin/env bash
  > set -euf -o pipefail
  > echo "dnf $*" >> "$DNF_LOG"
  > case " $* " in
  >   *" install "*|*" mark "*|*" autoremove "*) ;;
  >   *) echo "unexpected dnf command: $*" >&2; exit 9 ;;
  > esac
  > EOF
  $ chmod +x bin/dnf

  $ cat > bin/sudo << 'EOF'
  > #!/usr/bin/env bash
  > exec "$@"
  > EOF
  $ chmod +x bin/sudo

  $ cat > snapshot.sexp << 'EOF'
  > ((packages ((bash Group)
  >             (default-fonts-a Group)
  >             (git User)
  >             (htop Dependency)
  >             (libweak Weak_dependency)
  >             (stray User)
  >             (zlib Dependency)))
  >  (groups ((core (bash filesystem))
  >           (extra (extrapkg))
  >           (fonts (default-fonts-a))))
  >  (environments ((server-env (core))))
  >  (installed_groups (core fonts))
  >  (installed_environments (server-env)))
  > EOF
  $ export GKT_DNF_SNAPSHOT="$PWD/snapshot.sexp"
  $ echo '((removable ()) (protected ()))' > unneeded-none.sexp
  $ export GKT_DNF_UNNEEDED="$PWD/unneeded-none.sexp"

  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << 'EOF'
  > ((dnf ((packages (
  >    git
  >    htop
  >    libweak
  >    fzf
  >    (tlp (Only_hostname_regex "laptop-.*"))
  >    (server-env group))))))
  > EOF

Apply prints the plan but refuses to prompt without a tty.

  $ gkt dnf apply -hostname server-1
  warning: installed group is not in config: fonts
  install:
    fzf
  mark user:
    htop (Dependency)
    libweak (Weak Dependency)
  mark dependency:
    default-fonts-a (Group)
    stray (User)
  "stdin/stdout is not a tty: pass -yes to skip the confirmation prompt"
  [1]

With -yes it runs exactly the reconcile steps through sudo dnf; -offline
reaches each of them as --cacheonly.

  $ rm -f "$DNF_LOG"
  $ gkt dnf apply -yes -offline -hostname server-1 > /dev/null
  $ cat "$DNF_LOG"
  dnf --cacheonly install fzf
  dnf --cacheonly mark user htop libweak
  dnf --cacheonly mark dependency default-fonts-a stray

Dump omits installed ids that current comps no longer defines, as a comment,
so its output feeds straight back through -packages with no config needed.

  $ cat > snapshot-stale.sexp << 'EOF'
  > ((packages ((bash Group)
  >             (default-fonts-a Group)
  >             (git User)
  >             (htop Dependency)
  >             (libweak Weak_dependency)
  >             (stray User)
  >             (zlib Dependency)))
  >  (groups ((core (bash filesystem))
  >           (fonts (default-fonts-a))))
  >  (environments ())
  >  (installed_groups (core fonts))
  >  (installed_environments (server-env)))
  > EOF
  $ export GKT_DNF_SNAPSHOT="$PWD/snapshot-stale.sexp"
  $ gkt dnf dump | tee packages.sexp
  ; not in current comps, omitted: server-env
  (core Group)
  (fonts Group)
  git
  stray
  $ XDG_CONFIG_HOME="$PWD/empty" gkt dnf plan -packages "$PWD/packages.sexp" -hostname server-1
  warning: installed group is not in config: server-env
  nothing to do
  $ export GKT_DNF_SNAPSHOT="$PWD/snapshot.sexp"

The autoremove offer stands even when the reconcile has nothing to do, so a
declined autoremove resurfaces on the next run.

  $ cat > "$XDG_CONFIG_HOME/gk-tools.sexp" << 'EOF'
  > ((dnf ((packages (git stray (server-env group) (fonts group))))))
  > EOF
  $ echo '((removable (zlib)) (protected ()))' > unneeded-zlib.sexp
  $ rm -f "$DNF_LOG"
  $ GKT_DNF_UNNEEDED="$PWD/unneeded-zlib.sexp" gkt dnf apply -yes -hostname server-1
  nothing to do
  autoremove:
    zlib
  $ cat "$DNF_LOG"
  dnf autoremove
