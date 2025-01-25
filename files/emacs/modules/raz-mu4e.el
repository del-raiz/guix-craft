;; raz-mu4e.el --- Advanced eMail Management -*- lexical-binding: t; -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; FIXME: Configure GPG w/o having to use pinentry...


;; Code:

(use-package mu4e
  :disabled
  :if (eq system-type 'gnu/linux)
  :hook (mu4e-compose-mode . raz/mu4e-signature-hkf)
  :config
  ;; Signature Hook Function - autoloaded by use-package
  (defun raz/mu4e-signature-hkf ()
    "Insert the mu4e signature here, assuming it is a string."
    (interactive)

    (setq mu4e-compose-signature "\n\n -Erik")

    (save-excursion
      (when (stringp mu4e-compose-signature)
        (insert mu4e-compose-signature))))

  ;; Mu4e Base Settings
  (setq display-buffer-alist '(("\\*mu4e-main\\*" display-buffer-same-window))
        mu4e-mu-binary (executable-find "mu")
        mu4e-change-filenames-when-moving t
        mu4e-update-interval (* 10 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir (expand-file-name "~/.mail")
        sendmail-program (executable-find "msmtp")
        message-kill-buffer-on-exit t
        message-send-mail-function 'sendmail-send-it
        message-sendmail-envelope-from 'header
        message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        ;; Optional settings
        mu4e-confirm-quit nil
        mu4e-headers-visible-lines 20
        mu4e-headers-show-threads t
        mu4e-hide-index-messages t
        mu4e-headers-include-related nil
        org-mu4e-link-query-in-headers-mode nil)

  ;; Mail Accounts
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Fastmail"
          :enter-func
          (lambda () (mu4e-message "Enter erikalmaraz@fastmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave erikalmaraz@fastmail.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address     . "erikalmaraz@fastmail.com")
                  (user-full-name        . "Erik Almaraz")
                  (smtpmail-smtp-server  . "smpt.fastmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder    . "/Fastmail/Drafts")
                  (mu4e-sent-folder      . "/Fastmail/Sent")
                  (mu4e-refile-folder    . "/Fastmail/Archive")
                  (mu4e-trash-folder     . "/Fastmail/Trash")))))

  ;; Favorites
  (setq mu4e-maildir-shortcuts
        '(("/Fastmail/Inbox"  . ?i)
          ("/Fastmail/Archive". ?a)
          ("/Fastmail/Drafts" . ?d)
          ("/Fastmail/Sent"   . ?s)
          ("/Fastmail/Trash"  . ?t)
          ("/Fastmail/Home"   . ?h)
          ("/Fastmail/Work"   . ?w)
          ("/Fastmail/News"   . ?n))))




(provide 'raz-mu4e)
