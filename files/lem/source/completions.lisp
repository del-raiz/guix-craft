;;; ref - @vindarel (https://github.com/vindarel/lem-init)

(defpackage config/completions
  (:use :cl :lem))

(in-package :config/completions)


;; Choose the position of the completion prompt (new in May, 2024)
(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

;; and show the completion list directly, without a first press on TAB:
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))
