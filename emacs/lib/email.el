(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir "~/.mail"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder "/trash"
      mu4e-refile-folder "/archive")

(setq mu4e-compose-complete-addresses t)

(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addreses t
      mu4e-maildir-shortcuts '(("/me.craigfe.io" . ?p))

      user-mail-address "me@craigfe.io"
      user-full-name "Craig Ferguson"
      )

;; SMTP mail sending
