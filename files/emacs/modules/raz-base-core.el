;;;;; raz-base-core.el --- Base Config/Defaults -*- lexical-binding: t -*-

;;; Author: Erik P. Almaraz

;;; Commentary/References:
;;; Configuration of Emacs Core libraries.
;;; Default "Essential" Settings & Packages I use daily...
;;; See `comp.el' for review of Andrea Corallo's legendary world on native
;;; compilation (aka `eln' files).
;;; Research difference between emacs-next-tree-sitter & emacs-next-pgtk
;;; See https://www.emacswiki.org/emacs/PageBreaks
;;;  ‘forward-page’ (`C-x ]’ or `C-]’),
;;;  ‘backward-page’ (`C-x [’ or `C-[’), and `narrow-to-page' (‘C-x n p’).

;;; Code:
;;; File Settings: Auto Save, Backups, History, Bookmark, and Recent Files.


;;; Auto Mode Alist
;; create `custom' file extension for Xdefaults/Xresources file types...
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Associating-modes-with-files.html
;; (add-to-list 'auto-mode-alist '("\\.xconf\\'" . conf-xdefaults-mode))

;;; Auto Save: Prefix for generating auto-save-list-file-name
;; see - `auto-save-list-file-name'
(setq auto-save-list-file-prefix (expand-file-name "auto-save/.saves-"
                                                   *raz-var-directory*))
;; Backups
(setq  backup-directory-alist
       `(("." . ,(expand-file-name "backup" *raz-var-directory*)))
       make-backup-files t
       vc-make-backup-files nil
       backup-by-copying t
       version-control t
       delete-old-versions t
       kept-old-versions 6
       kept-new-versions 9
       delete-by-moving-to-trash t)

;;; History
(use-package savehist
  :diminish savehist-mode
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (expand-file-name "savehist.el" *raz-var-directory*))
  :config
  (setq history-length 500
        history-delete-duplicates t)
  (savehist-mode 1))

;;; Bookmarks
(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" *raz-var-directory*)))

;;; Recent Files
(use-package recentf
  ;; recentf settings/hack
  ;; TODO: Optimize use-package configuration for this!
  :diminish recentf-mode
  :init
  (defun raz/advice-no-msg (orig &rest args)
    "Docstring tbd..."
    ;; Dynamic Scoping to the rescue.
    (let ((inhibit-message t))
      (apply orig args)))
  (setq recentf-save-file (expand-file-name "recentf" *raz-var-directory*)
        recentf-max-menu-items 50)
  ;; (customize-set-variable 'recentf-exlcude)

  ;; Makes a call to `load' which calls `message' because it's third argument is nil,
  ;; telling `load' to call `message'
  (advice-add 'recentf-cleanup :around #'raz/advice-no-msg)
  (advice-add 'recentf-load-list :around #'raz/advice-no-msg)
  :config
  (recentf-mode))


;;; Coding/Editing Defaults

;; .dir-local variables for development projects
(setq enable-local-eval nil)
(setq enable-local-variables nil)

(set-default-coding-systems 'utf-8)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq-default ;cursor-type 'bar

              large-file-warning-threshold 100000000
              find-file-visit-truename t)
(global-auto-revert-mode 1)
(delete-selection-mode)
(column-number-mode 1)

(use-package display-fill-column-indicator
  ;; TODO: Customize theme color for this element -> via ':config' keyword
  :diminish
  ;; Only activate for lisp-mode -> stumpwm-contrib preference
  :hook ((lisp-mode . display-fill-column-indicator-mode)
         (scheme-mode . display-fill-column-indicator-mode))
  :custom
  (fill-column 94)
  (display-fill-column-indicator-column fill-column))

(defun raz/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "C-c o" 'raz/switch-to-minibuffer)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;;Dired setup
(use-package dired-x
  ;; Set dired-x buffer-local variables here.  For example:
  ;; (dired-omit-mode 1)
  :disabled
  :after dired)

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





(provide 'raz-base-core)
