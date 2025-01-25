;; early-init.el - Emacs Early Initialization File -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary:

;; Code:


;;
;; PATHs, Environment & Configuration Variables
;;

;; Set GUIX packages to Emacs Load Path
(defvar raz/guix-emacs-packages-path
  (expand-file-name "~/.guix-home/profile/share/emacs/site-lisp/")
  "Guix Home Profile Emacs Packages PATH.")

(when (eq system-type 'gnu/linux)
  ;; Set some guix specific configurations...
  (load (concat raz/guix-emacs-packages-path "subdirs.el") :no-error :no-message)
  (add-to-list 'load-path raz/guix-emacs-packages-path))

;; Variables/Functions defining configuration
(defvar *raz/load-custom-file* nil
  "When non-nil, load `custome.el' after user's config file, `config.el'.")


;;
;; Compilation Settings
;;

(setq load-prefer-newer t
      warning-suppress-log-types '((comp) (initialization))
      warning-suppress-types '((initialization)))

(when (featurep 'native-compile)
  ;; Set native compilation asynchronous
  (setq native-comp-jit-compilation t
        native-comp-async-report-warnings-errors nil)
  ;; Set the right directory to store the native compilation cache
  ;; NOTE: The method for setting the eln-cache directory depends on the emacs version
  ;; This is disregarded here - assume I always use emacs-next, i.e. .ge. 29 for now.
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))


;;
;; Performance Optimizations/Tweaks
;;

;; Disable Dialogs/Echos/Bells & Startup Frames/Screens/Buffers
(setq-default init-file-user user-login-name
              frame-inhibit-implied-resize t
              ring-bell-function 'ignore
              use-file-dialog nil
              use-dialog-box t
              inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-buffer-menu t)

;; Inhibit redisplay & messaging/dialog/echo to avoid flickering
;; loading/compiling upon iniial startup etc.
(setq inhibit-redisplay t
      inhibit-message t)

;; restore messages & redisplay (after initialization is complete)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  inhibit-message nil)
            (redisplay)))

;; Garbage Collection *optimization*
;; Temporarily increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes (* 8 100 1000).
;; Reset GC to default after start-up
(defvar raz/gc-cons-threshold gc-cons-threshold
  "Capture the default value of `gc-cons-threshold' for restoration.")
(setq gc-cons-threshold most-positive-fixnum)

;; ;; Temporarily disable file-handling during startup.
(defvar raz/file-name-handler-alist file-name-handler-alist
  "Capture the default value of `file-name-handler-alist' for restoration.")
(setq file-name-handler-alist nil)

;; Temporarily disable `vc-handled-backends' for I/O optimization.
(defvar raz/vc-handled-backends vc-handled-backends
  "Capture the default value of `vc-handled-backends' for restoration.")
(setq vc-handled-backends nil)

;; Retore original GC & file-handling/VC alist defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq  file-name-handler-alist raz/file-name-handler-alist)

            (setq gc-cons-threshold raz/gc-cons-threshold
                  vc-handled-backends raz/vc-handled-backends)))


;;
;; Set Frame Parameters & UI/UX Configuration Variables
;;

;; See Window Frame parameters
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html
(set-frame-name "Home")

;; Customize Frame Title Construct
(setq-default frame-title-format
              '(multiple-frames
                "%b"
                ("" "%b @" user-login-name)))

(setq frame-resize-pixelwise t)

(setq raz/custom-frame-alist
      '(;;(alpha-background . 85) ; doesn't work on X11
        ;;(undecorated . t)
        ;;(maximized . t)
        (alpha . (85 . 85))
        (use-frame-synchronization . t)))

(setq initial-frame-alist
      (append
       raz/custom-frame-alist
       initial-frame-alist))

(setq default-frame-alist
      (append
       raz/custom-frame-alist
       default-frame-alist))

;; Set Initial UI/UX Configuration for a clean startup experience
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(pixel-scroll-precision-mode 1)

;; Avoid the flash of light
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/early-init.el
(defun raz/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed - use Nord theme colors."
  (setq mode-line-format nil)
  (set-face-attribute 'default nil
                      :background "#000000" :foreground "#d8dee9")
  (set-face-attribute 'mode-line nil
                      :background "#000000" :foreground "#d8dee9" :box 'unspecified)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#000000" :foreground "#d8dee9" :box 'unspecified))

(raz/avoid-initial-flash-of-light)


;;
;; Package Management System & Loading Preferences
;;

(setq package-enable-at-startup t)

;; TODO - Determine how to handle package loading between
;;         `GUIX' and `package.el' and `use-package' abilities to leverage both
;;         systems.
;; https://www.reddit.com/r/emacs/comments/jhb2i6/
;; guix_the_right_way_to_manage_your_packages/

;; Guix Home PATH (for reference)
;; (load-file "~/.guix-home/profile/share/emacs/site-lisp/subdirs.el")
;; For Guix system (for reference) - `use-package' seems to handle this...
;; (load-file "/run/current-system/profile/share/emacs/site-lisp/subdirs.el")

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             :append)

;; MELPA Stable - see `package-archive-priorities` and `package-pinned-packages`.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             :append)

(package-initialize)
