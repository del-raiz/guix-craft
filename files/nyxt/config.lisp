;;;; Nyxt Configuration - Initialization File

;;; Commentary:
;;; Set Buffer Settings & load in Keepassxc interface configuration as well as
;;; modified invader theme as an extension (similar to StumpWM contrib modules).

;;; References
;;; 1. https://github.com/aartaka/nyxt-config/
;;; 2. https://discourse.atlas.engineer/t/where-is-the-download-directory-specified/285
;;;    Set XDG_DOWNLOAD_DIR in start-stumpwm.sh -> should define custom XDG env vars there!
;;;    see: nyxt:describe-function?fn=%1Bxdg-download-dir&function=%1Bxdg-download-dir


;;; A very simple configuration... doesn't need to be complex...

(in-package :nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere... Doesn't seem to be needed anymore...
;; #+(or nyxt-3 nyxt-4) (reset-asdf-registries)

(define-configuration buffer
  ((default-modes `(emacs-mode ,@%slot-value%))))

;; Loading files from the same directory (~/.config/nyxt/).
(define-nyxt-user-system-and-load nyxt-user/basic-config
  :description "Nyxt Interface Configuration."
  :components ("keepassxc-pwi"
               "keepassxc-3431"))


;;; Nyxt Extensions

;; Borrowed from aartaka (see #:ref-2)
(defmacro defextsystem (system &optional file)
  "Helper macro to load configuration for extensions.
Loads a newly-generated ASDF system depending on SYSTEM.
FILE, if provided, is loaded after the generated system successfully
loads."
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) ,@(when file `(:components (,file)))))

;; ~/.local/share/nyxt/extensions/*
(defextsystem :nx-invader-2)
(defextsystem :nx-micros)
(defextsystem :nx-code)

;; (define-nyxt-user-system-and-load nyxt-user/nx-invader-2-proxy
;;   :description "Simple Dark style theme for Nyxt"
;;   :depends-on ("nx-invader-2"))

;; (nyxt:define-nyxt-user-system-and-load nyxt-user/nx-micros-proxy
;;   :description "Connect Nyxt to Lem via Micros."
;;   :depends-on ("nx-micros"))

;; (define-nyxt-user-system-and-load nyxt-user/nx-code-proxy
;;   :description "Modern Emacs-like Editor/IDE for Nyxt."
;;   :depends-on ("nx-code"))


;;; Hacks

;; Borrowed from aartaka
(defmethod files:resolve ((profile nyxt:nyxt-profile)
                          (file nyxt/mode/bookmark:bookmarks-file))
  "Reroute bookmarks to the `.config/nyxt/' directory."
  #p"~/.config/nyxt/bookmarks.lisp")
