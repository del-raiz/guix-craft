;;;; password-dev.lisp

;;; Fix bug with save-new-password, fails to connect to database unless ether
;;; copy-username or copy-password has been invoked first

(in-package #:nyxt/mode/password)

(define-command save-new-password-dev (&optional (buffer (current-buffer)))
  "Save password to password interface."
  (password-debug-info)
  (cond
    ((and (password-interface buffer)
          (nyxt:has-method-p (password-interface (find-submode 'password-mode buffer))
                             #'password:save-password))
     ;;See nyxt/source/mode/password.lisp:L159
     ;;Not sure why this wasn't implemented similarily to `copy-password' and
     ;;`copy-username' in this regard.
     (with-password (password-interface buffer)
       (let* ((password-name (prompt1
                              :prompt "Name for new password"
                              :input (or (quri:uri-domain (url (current-buffer))) "")
                              :sources 'prompter:raw-source))
              (new-password (prompt1
                             :prompt "New password (leave empty to generate)"
                             :invisible-input-p t
                             :sources 'prompter:raw-source))
              (username (prompt1
                         :prompt "Username (can be empty)"
                         :sources 'prompter:raw-source)))
         (password:save-password (password-interface buffer)
                                 :username username
                                 :password-name password-name
                                 :password new-password))))
    ((null (password-interface buffer))
     (echo-warning "No password manager found."))
    (t (echo-warning "Password manager ~s does not support saving passwords."
                     (string-downcase
                      (class-name (class-of (password-interface buffer))))))))

;; Currently not working - how to override existing method? Or change existing method.
;; (defmethod password::execute :before ((password-interface password:keepassxc-interface) (arguments list)
;;                                       &rest run-program-args &key &allow-other-keys)
;;   (declare (ignore arguments run-program-args))
;;   (cond
;;     ((eql (password::yubikey-slot password-interface) "disabled")
;;      (echo "Yubikey disabled."))
;;     ((password::yubikey-slot password-interface)
;;      (echo "Tap your Yubikey to prove KeePassXC database access"))))
