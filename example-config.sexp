;; gk-tools.sexp — drop into $XDG_CONFIG_HOME/gk-tools.sexp (typically
;; ~/.config/gk-tools.sexp). Every section is optional; omit any subcommand
;; you don't configure.
;;
;; The config is processed by sexp_macro, so the standard macro forms
;; (:let, :use, :concat, :include) work. The example below uses :let to
;; define the calendar host once and reuses it for both [server_url] and
;; the rbw lookup in [password].
(:let host () cal.example.com)

((caldav_upload (
   (server_url (:concat https:// (:use host) /calendars/user/personal/))
   ;; The CLI flags (-server-url, -username, -password) override these values.
   (username user@example.com)
   (password (command (:concat "rbw get " (:use host))))))
 ;; Repos for [gkt git managed-sync]: all are synced when run with no
 ;; arguments; an id selects one.
 (git_managed_sync ((
   repos (
     ((id repo-a) (dir ~/repo-a))
     ((id repo-b) (dir /path/to/repo-b))))))
 ;; Remote for [gkt notmuch setup]; [gkt notmuch sync] needs no config.
 (notmuch_git ((remote git@git.example.com:notmuch-tags.git)))
 (llm_completions (
   (url https://api.example.com/v1/responses)
   ;; Omit token for unauthenticated endpoints.
   (token (env LLM_GATEWAY_TOKEN))
   ;; (token (command "rbw get llm-gateway-token"))
   ;; (token (literal "your-gateway-token"))
   (model             your-model-alias)
   ;; Applies before caller-specific instructions.
   (system_prompt "Give concise, factual answers.")
   (reasoning_effort  low)
   ;; (reasoning_summary auto)
   (max_output_tokens 4096)
   ;; (temperature 0.2)
   (timeout           2m)))
 ;; [gkt misc classify-mail -dry-run -folder INBOX -file /mail/account/INBOX/cur/message]
 (classify_mail (
   (system_prompt "These are personal email accounts. Leave correspondence in place unless a destination clearly fits.")
   (destinations (
     ((folder      Newsletters)
      (description "Newsletters and mailing list digests"))
     ((folder Receipts)
      (description "Receipts, invoices, and order confirmations")))))))
