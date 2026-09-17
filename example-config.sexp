;; gk-tools.sexp — drop into $XDG_CONFIG_HOME/gk-tools.sexp (typically
;; ~/.config/gk-tools.sexp). Every section is optional; omit any subcommand
;; you don't configure.
;;
;; The config is processed by sexp_macro, so the standard macro forms
;; (:let, :use, :concat, :include) work. The example below uses :let to
;; define the calendar host once and reuses it for both [server_url] and
;; the rbw lookup in [password_cmd].

(:let host () "cal.example.com")

((caldav_upload (
  (server_url (:concat "https://" (:use host) "/calendars/user/personal/"))

  ;; Pick ONE of [username]+[password], or [password_cmd]. The CLI flags
  ;; (-server-url, -username, -password) override these values; the aerc
  ;; accounts.conf fallback fills in anything still unset.
  (username "user@example.com")
  (password_cmd (:concat "rbw get " (:use host)))))

 ;; Packages for [gkt dnf]: what should be explicitly installed (dnf reasons
 ;; User and Group). Entries are NAME or (NAME MODIFIER ...): [group] marks a
 ;; comps group/environment whose members are protected from demotion;
 ;; [(Only_hostname_regex RE)] scopes an entry to hosts whose full hostname
 ;; matches RE (PCRE; leading ! inverts). [gkt dnf dump] prints entries
 ;; covering the current box. Keep the list in its own file with
 ;; (:include dnf-packages.sexp) here, or pass it via [-packages FILE].
 (dnf ((packages (git
                  htop
                  (tlp (Only_hostname_regex "laptop-.*"))
                  (cloud-server-environment group)))))

 ;; Repos for [gkt git managed-sync]: all are synced when run with no
 ;; arguments; an id selects one.
 (git_managed_sync ((repos (((id repo-a) (dir ~/repo-a))
                            ((id repo-b) (dir /path/to/repo-b))))))

 ;; Remote for [gkt notmuch setup]; [gkt notmuch sync] needs no config.
 (notmuch_git ((remote git@git.example.com:notmuch-tags.git))))
