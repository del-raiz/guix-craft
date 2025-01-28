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
;;; everywhere.
#+(or nyxt-3 nyxt-4) (reset-asdf-registries)

(define-configuration buffer
  ((default-modes `(emacs-mode ,@%slot-value%))))

;; Loading files from the same directory (~/.config/nyxt/).
(define-nyxt-user-system-and-load nyxt-user/basic-config
  :description "Nyxt Interface Configuration."
  :components ("keepassxc-pwi"
               "keepassxc-3431"))

;;; Nyxt Extensions

;; ~/.local/share/nyxt/extensions/*
(define-nyxt-user-system-and-load nyxt-user/nx-invader-2-proxy
  :description "Simple Dark style theme for Nyxt"
  :depends-on ("nx-invader-2"))


;;; Hacks

;;Borrowed from aartaka
(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
           "Reroute bookmarks to the `.config/nyxt/' directory."
           #p"~/.config/nyxt/bookmarks.lisp")

;; Setup Micros Server to connect to Lem.

;; (define-nyxt-user-system-and-load nyxt-user/commands
;;   :description "Custom Nyxt Commands."
;;   :components ("commands"))

(in-package :nyxt)
(require :micros)

(defvar *micros-port* 4006
  "Default Common Lisp server port for Nyxt")

;; Lem connection to Nyxt

(define-command start-micros (&optional (micros-port *micros-port*))
    "Start a Micros server enabling connecting to Lem via SLIME."
  (micros:create-server :port micros-port :dont-close t)
  (echo "Micros server started at port ~a" micros-port))

(define-command stop-micros (&optional (micros-port *micros-port*))
    "Stop current Micros server."
  (micros:stop-server micros-port)
  (echo "Closing Micros server at port ~a" micros-port))

