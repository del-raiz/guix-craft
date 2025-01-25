;; raz-guile-ide.el --- Guile IDE via Ares/Arei -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References
;; 1. nREPL RPC server - https://git.sr.ht/~abcdw/guile-ares-rs
;; 2. Emacs nREPL bindings - https://git.sr.ht/~abcdw/emacs-arei
;; 3. Sesman - https://github.com/vspinu/sesman?tab=readme-ov-file
;; 4. Corfu (COmpletions in Region FUnction) - https://github.com/minad/corfu
;; 5.

;; TODO: Tell ares-rs where to search for Guix stuff
;; it may work if you set `export
;; GUILE_LOAD_PATH=$GUIX_ENVIRONMENT/share/guile/site/3.0`

;; Code:

(use-package arei
  :if (eq system-type 'gnu/linux)
  ;; :hook ((scheme-mode . raz/start-guile-ares))
  :config
  (setq geiser-mode-auto-p nil)

  (defvar *raz/ares-rs-process* nil
    "Holds process for Ares RS nREPL RPC server.")

  (defun raz/kill-ares ()
    "Kill Ares RS nREPL RPC server."
    (interactive)
    (when *raz/ares-rs-process*
      (ignore-errors
        (kill-process *raz/ares-rs-process*))
      (setq *raz/ares-rs-process* nil)))

  (defun raz/start-guile-ares ()
    "Start Ares RS nREPL RPC server."
    (interactive)
    ;; FIXME - kills session every time a new file is visited...
    (raz/kill-ares)
    (setq *raz/ares-rs-process*
          (start-process-shell-command
           "Ares nREPL" nil
           ;; guile -c '((@ (nrepl server) run-nrepl-server) #:port 7888)'
           (concat "guile -c "
                   "'((@ (nrepl server) run-nrepl-server) "
                   "#:port 7888)'"))))

  ;;FIXME - Translate code to work with `sessman-start'
  ;; Pass as hook to `use-package'
  (defun raz/arei-auto-connect-nrepl ()
    (unless *raz/ares-rs-process*
      (save-excursion (sesman-start)))))

(use-package sesman
  :if (eq system-type 'gnu/linux))


;; Guix install of `emacs-guix' comes with:
;; emacs-bui, emacs-dash, emacs-edit-indirect,
;; emacs-geiser, emacs-geiser-guile, emacs-magit-popup
;; module-import-compiled
(use-package guix
  :if (eq system-type 'gnu/linux)
  :config
  (setq geiser-mode-auto-p nil))





(provide 'raz-guile-ide)
