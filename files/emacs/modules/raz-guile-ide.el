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

;; Set default to guile.
(customize-set-variable 'scheme-program-name "guile")

;; .dir-local variables for development projects
(setq enable-local-eval nil)
(setq enable-local-variables nil)
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq-default  sentence-end-double-space t)
;; (setq-default cursor-type 'bar)

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package display-fill-column-indicator
  ;; TODO: Customize theme color for this element -> via ':config' keyword
  :diminish
  ;; Only activate for scheme-mode -> guix preference
  :hook (scheme-mode . display-fill-column-indicator-mode)
  :custom
  (fill-column 78) ;; Guix Standard (why?
  (display-fill-column-indicator-column fill-column)
  :config
  ;; Make fill-column-indicator face the darker --> line-number face
  ;; theme value #5c5e5e --> #3f4040 (good with doom-tomorrow-night theme)
  (raz/set-face-attribute 'fill-column-indicator '(:foreground "#3f4040")))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  ;; Save & Restore Window configuration
  ;; https://www.emacswiki.org/emacs/EdiffMode
  (add-hook
   'ediff-load-hook
   (lambda ()
     (add-hook 'ediff-before-setup-hook
               (lambda ()
                 (setq ediff-saved-window-configuration
                       (current-window-configuration))))
     (let ((restore-window-configuration
            (lambda ()
              (set-window-configuration ediff-saved-window-configuration))))
       (add-hook 'ediff-quit-hook
                 restore-window-configuration
                 'append)
       (add-hook 'ediff-suspend-hook
                 restore-window-configuration
                 'append)))))

(use-package prettify-symbols
  :disabled
  ;; Keep here for reference.
  ;; :hook (scheme-mode . prettify-symbols-mode)
  )

;;
;; External Packages
;;

(use-package magit
  :defer 5
  :custom
  (magit-clone-always-transient nil)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (vc-follow-symlinks t))

(use-package paredit
  :diminish paredit-mode
  :hook ((eval-expression-minibuffer-setup
          lisp-interaction-mode
          emacs-lisp-mode
          lisp-mode
          scheme-mode) . enable-paredit-mode))

(use-package vterm
  ;; Most of vterms configuration is done in .bashrc
  ;; see ~/.dotfiles/config/home/dot-bashrc.sh
  :commands vterm
  :hook (vterm-mode . set-vterm-font)
  :config
  ;; TODO set prompt to λ ?
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
        vterm-copy-exclude-prompt t
        vterm-kill-buffer-on-exit t
        vterm-max-scrollback 2000)

  (defun set-vterm-font ()
    "Set custom font for vterm."
    (set (make-local-variable 'buffer-face-mode-face)
         '(:family "Hack" :height 110))
    (buffer-face-mode t)))

;; Guix install of `emacs-guix' comes with:
;; emacs-bui, emacs-dash, emacs-edit-indirect,
;; emacs-geiser, emacs-geiser-guile, emacs-magit-popup
;; module-import-compiled
(use-package guix)

(use-package geiser
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (geiser-mode-auto-p t)
  (geiser-mode-autodoc-p t)
  (geiser-repl-per-project-p t))

(use-package geiser-guile
  :config
  ;; (add-to-list 'geiser-guile-load-path "~/Work/ref/guix")
  )

;;
;; Experimental
;;
(use-package arei
  ;; :hook ((scheme-mode . raz/start-guile-ares))
  :bind (("C-c g" . raz/start-guile-ares)
         ("C-c k" . raz/kill-guile-ares))
  :config
  (defvar *raz/ares-rs-process* nil
    "Holds process for Ares RS nREPL RPC server.")

  (defun raz/start-guile-ares ()
    "Start Ares RS nREPL RPC server."
    (interactive)
    (setq geiser-mode-auto-p nil)
    ;; FIXME - kills session every time a new file is visited...
    (raz/kill-guile-ares)
    (message "Spawning guile-ares-rs.")
    (setq *raz/ares-rs-process*
          (start-process-shell-command
           "Ares nREPL" nil
           ;; guile -c '((@ (nrepl server) run-nrepl-server) #:port 7888)'
           (concat "guile -c "
                   "'((@ (nrepl server) run-nrepl-server) "
                   "#:port 7888)'"))))

  (defun raz/kill-guile-ares ()
    "Kill Ares RS nREPL RPC server."
    (interactive)
    (when *raz/ares-rs-process*
      (message "Killing guile-ares-rs.")
      (ignore-errors
        (kill-process *raz/ares-rs-process*))
      (setq *raz/ares-rs-process* nil)))

  ;;FIXME - Translate code to work with `sessman-start'
  ;; Pass as hook to `use-package'
  (defun raz/arei-auto-connect-nrepl ()
    (unless *raz/ares-rs-process*
      (save-excursion (sesman-start))))


  ) ;; Guile ares IDE still a WIP...




(provide 'raz-guile-ide)
