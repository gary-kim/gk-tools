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
  (password_cmd (:concat "rbw get " (:use host))))))
