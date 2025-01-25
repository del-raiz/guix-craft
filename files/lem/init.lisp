;;;; init.lisp - Lem Initialization File

;;; Erik P Almaraz (aka logoraz)
;;; Ref: https://github.com/fukamachi/.lem
;;; Ref: https://codeberg.org/sasanidas/lem-config/

(defpackage :lem-config
  (:use :cl :lem))
(in-package :lem-config)

;; Load my init source files.
(let ((asdf:*central-registry*
        (cons #P"~/.config/lem/" asdf:*central-registry*)))
  (asdf:load-system :lem-config))

