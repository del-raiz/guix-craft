;;; Borrowed from @gavinok (https://github.com/Gavinok/.lem)

(defpackage config/paredit
  (:use :cl :lem)
  (:export))

(in-package :config/paredit)


(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer) 'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

;; Paredit Mappings
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Right"
  'lem-paredit-mode:paredit-slurp)
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Left"
  'lem-paredit-mode:paredit-barf)

;; FIXME - Seems to be causing an error
(define-command paredit-quote-wrap () ()
  (progn
    (lem-paredit-mode:paredit-insert-doublequote)
    (lem-paredit-mode:paredit-slurp)
    (lem:delete-next-char)))

(define-key lem-paredit-mode:*paredit-mode-keymap* "M-\"" 'paredit-quote-wrap)

