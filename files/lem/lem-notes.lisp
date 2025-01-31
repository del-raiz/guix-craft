;;;; notes.lisp --> lem

(define-command backward-kill-word-or-region (start end n) (:region :universal)
  "Kill word or region."
  (if (mark-active-p (buffer-mark-object (current-buffer)))
      (kill-region start end)
      (backward-delete-word n)))
(define-key *global-keymap* "C-w" 'backward-kill-word-or-region)
