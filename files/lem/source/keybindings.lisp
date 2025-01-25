(defpackage config/keybindings
  (:use :cl :lem))

(in-package :config/keybindings)


;; Make undo & redo what I am used to
(defun custom-keybindings ()
  "Defining in a function to re-deploy after starting lem/legit after init."
  (define-key *global-keymap* "C-/" 'undo)
  (define-key *global-keymap* "C-_" 'redo)

  (define-key *global-keymap* "C-h B" 'describe-bindings)
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h a" 'apropos-command)
  (define-key *global-keymap* "C-h p" 'lem-lisp-mode:lisp-apropos-package)
  (define-key *global-keymap* "C-x F" 'lem-core/commands/file:find-file-recursively))

(custom-keybindings) ; Enable custom keybindings on initialization.
