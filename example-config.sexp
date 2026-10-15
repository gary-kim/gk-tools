;; $XDG_CONFIG_HOME/gk-tools.sexp (typically ~/.config/gk-tools.sexp).
;; All sections are optional; sexp_macro forms (:let, :use, :concat, :include) are supported.
(:let host () cal.example.com)

((caldav_upload (
   (server_url (:concat https:// (:use host) /calendars/user/personal/))
   ;; Overridden by -server-url, -username, and -password.
   (username user@example.com)
   (password (command (:concat "rbw get " (:use host))))))
 ;; Repos for [gkt git managed-sync]. Syncs all by default, or those selected by -id.
 (git_managed_sync ((
   repos (
     ((id repo-a) (dir ~/repo-a))
     ((id repo-b) (dir /path/to/repo-b))))))
 ;; Remote for [gkt notmuch setup]; [gkt notmuch sync] needs no config.
 (notmuch_git ((remote git@git.example.com:notmuch-tags.git)))
 (llm_completions (
   (url https://api.example.com/v1/responses)
   ;; Omit for unauthenticated endpoints.
   (token (env LLM_GATEWAY_TOKEN))
   ;; (token (command "rbw get llm-gateway-token"))
   ;; (token (literal "your-gateway-token"))
   (model             your-model-alias)
   ;; Prepended to caller-specific instructions.
   (system_prompt "Give concise, factual answers.")
   (reasoning_effort  low)
   ;; (reasoning_summary auto)
   (max_output_tokens 4096)
   ;; (temperature 0.2)
   (timeout           2m)))
 (classify_mail (
   (system_prompt "These are personal email accounts. Leave correspondence in place unless a destination clearly fits.")
   (destinations (
     ((folder      Newsletters)
      (description "Newsletters and mailing list digests"))
     ((folder Receipts)
      (description "Receipts, invoices, and order confirmations")))))))
