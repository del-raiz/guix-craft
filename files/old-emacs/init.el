;; init.el - Emacs Initialization File -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary:
;; TODO: change del-emacs-setq-customized -- setq (more efficient)

;; Code:



;; Define:= Cusotomizations UI for Del Emacs.

(defgroup raz nil
  "Del Emacs"
  :tag "raz Emacs"
  :link '(url-link "https://github.com/logoraz/guix-craft")
  :group 'emacs)

(defvar *raz-var-directory* (expand-file-name "var/" user-emacs-directory)
  "Default var directory.")

(defvar *raz-etc-directory* (expand-file-name "etc/" user-emacs-directory)
  "Default etc directory.")

(defvar *raz-elisp-directory* (expand-file-name "elisp/" user-emacs-directory)
  "Default Emacs Lisp directory")

(defvar *raz-modules-directory* (expand-file-name "modules/"
                                                  user-emacs-directory)
  "Default Emacs Modules directory.")

;; Add the elisp & modules directories to the load path
(add-to-list 'load-path *raz-elisp-directory*)
(add-to-list 'load-path *raz-modules-directory*)

;; Set custom file to NOT be our init file.
(setq custom-file (expand-file-name "custom.el" *raz-etc-directory*))

(setq inhibit-startup-echo-area-message user-login-name)

(when *raz/load-custom-file*
  (load custom-file t :no-error :no-message))

(when (eq system-type 'gnu/linux)
  (guix-emacs-autoload-packages))

(if (eq system-type 'windows-nt)
    (set-frame-parameter nil 'undecorated nil))



;; Enable `use-package' statistics - must be set before any `use-package' forms.
;; Run command M-x `use-package-report' to see
;; 1. How many packages were loaded,
;; 2. What stage of initialization they've reached,
;; 3. How much aggregate time they've spend (roughly).
(setq use-package-compute-statistics :enable)

;; Define:= Modules & User Configurations
;; TODO - Add `which-key' as this is staged to be added to core...

;; Load Modules
;; Macros/Helper functions
(require 'raz-subrx)
;; Base + IDE
(require 'raz-base-core)
(require 'raz-base-ext)
(require 'raz-completions-mct)
(require 'raz-lisp-ide)
(require 'raz-guile-ide)
;; Notes/Office/Mail/Chat
(require 'raz-denote)
(require 'raz-org)
(require 'raz-erc)
;;TODO - need to configure on new GUIX Home scaffold
;; (require 'raz-mu4e)
