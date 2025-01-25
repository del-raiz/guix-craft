;;;; Nyxt Configuration - Initialization File

;;; Commentary:
;;; Set Mode & Browser Settings

;;; References
;;; 1. https://github.com/aartaka/nyxt-config/
;;; 2. https://discourse.atlas.engineer/t/where-is-the-download-directory-specified/285
;;;    Set XDG_DOWNLOAD_DIR in start-stumpwm.sh -> should define custom XDG env vars there!
;;;    see: nyxt:describe-function?fn=%1Bxdg-download-dir&function=%1Bxdg-download-dir
;;; 3. TBD
;;; 4. TBD


;;; Start-Up & Configuration

(in-package #:nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere.
#+(or nyxt-3 nyxt-4) (reset-asdf-registries)

;; Loading files from the same directory (~/.config/nyxt/).
(define-nyxt-user-system-and-load nyxt-user/basic-config
  ;; :config-directory (#P"~/.config/nyxt/modules/")
  :components ("utilities"
               "passwords"
               "passwords-dev"))

;; Base broswer/buffer configurations
(define-configuration :browser
    ((restore-session-on-startup-p nil)))

(define-configuration :buffer
    ((default-modes `(emacs-mode ,@%slot-value%))))

;; Drastically impacts Nyxt startup...
(define-configuration :web-buffer
    ((default-modes `(blocker-mode ,@%slot-value%))))

;; trialing out
(define-configuration :document-buffer
    ((search-always-auto-complete-p nil)))

;; Borrowed from aartaka
(define-configuration :prompt-buffer
    ((dynamic-attribute-width-p t)))

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
  "Reroute bookmarks to the `.config/nyxt/' directory."
  #p"~/.config/nyxt/bookmarks.lisp")


;;; Nyxt Extensions
;;; ~/.local/share/nyxt/extensions/*
(define-nyxt-user-system-and-load nyxt-user/nx-invader-2-proxy
  ;; :extensions-directory (#P"~/.config/nyxt/extensions/")
  :description "Dark style theme for Nyxt"
  :depends-on ("nx-invader-2"))

