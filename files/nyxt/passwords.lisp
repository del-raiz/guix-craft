;;;; Nyxt Passwords Modules

;;; Commentary:
;;; Note: Inorder to activate and be able to save new passwords, `save-new-password',
;;; you need to first instantiate via `copy-passowrd' or `copy-username'

;;; References
;;; 1. https://nyxt.atlas.engineer/documentation#keepassxc-support
;;; 2. https://github.com/aartaka/nyxt-config/tree/master
;;; 3. Look into `sera:resolve-executable'
;;;    (sera:resolve-executable "keepassxc-cli")

(in-package #:nyxt-user)


(defmethod initialize-instance :after ((interface password:keepassxc-interface)
                                       &key &allow-other-keys)
  (setf (password:password-file interface) "/home/logoraz/Documents/moses/p.kdbx"
        (password:key-file interface) "/home/logoraz/Documents/moses/pkf"
        (password:yubikey-slot interface) "")) ; set as `empty string' to avoid propmt

(define-configuration nyxt/mode/password:password-mode
    ((password-interface (make-instance 'password:keepassxc-interface))))

(define-configuration buffer
    ((default-modes `(password-mode ,@%slot-value%))))
