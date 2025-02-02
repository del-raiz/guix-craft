;;;; StumpWM Initialization File (config.lisp --> config)

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)


;;; Set PATHs: modules & data directories, etc.

;; Define Guix profiles
(defconstant +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defconstant +guix-home-path+ "/home/logoraz/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defconstant +xdg-data-home-path+ (concat (getenv "XDG_DATA_HOME") "/") 
  "Define XDG_DATA_HOME PATH.")

(defconstant +xdg-cache-home-path+ (concat (getenv "XDG_CACHE_HOME") "/") 
  "Define XDG_CACHE_HOME PATH.")

(defconstant +swm-data-dir+ (concat (getenv "XDG_CACHE_HOME")
                                    "/stumpwm/")
  "Define StumpWM Data Dir PATH.")


;; Set StumpWM modules directory - at system level!
(set-module-dir (concat +guix-home-path+
                        "common-lisp/sbcl/"))

;; Set StumpWM data directory
;; https://stumpwm.github.io/git/stumpwm-git_67.html
;; Turn's out you can't change creation of `~/.stumpwm.d' as it is hard coded into
;; stumpwm's initialization process -> created before `load-rc-file', that is, before
;; the user's config is read in...
;; Ref: https://github.com/stumpwm/stumpwm/blob/master/stumpwm.lisp#L262
(defun custom-data-dir ()
  (merge-pathnames ".cache/stumpwm/" (user-homedir-pathname)))

(defun ensure-custom-data-dir ()
  (ensure-directories-exist (custom-data-dir) :mode #o700))

(ensure-custom-data-dir)
(setf *data-dir* (custom-data-dir))

;; Set StumpWM as default package
(setf *default-package* :stumpwm)

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

;;; Import asdf-loads
;; Load ~/common-lisp systems/packages
;; (let ((asdf:*central-registry*
;;         (cons #P"~/common-lisp/" asdf:*central-registry*)))
;;   (asdf:load-system :micros))

;; ref: https://www.github.com/aartaka/stumpwm-config
;; (asdf:load-systems :slynk :swank)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;; TODO: Determine a better way to load in modules...
;; sort of just doing things the primitive way.

;;; Initialize X11 Desktop Environment & Resources.
(load "~/.config/stumpwm/source/auto-start.lisp")


;;; Load in configuration source files
(load "~/.config/stumpwm/source/syntax.lisp")
(load "~/.config/stumpwm/source/utilities.lisp")
(load "~/.config/stumpwm/source/colors.lisp")
(load "~/.config/stumpwm/source/theme.lisp")
(load "~/.config/stumpwm/source/frames.lisp")
(load "~/.config/stumpwm/source/keybindings.lisp")
(load "~/.config/stumpwm/source/modeline.lisp")

;; Start the mode line
(when *initializing*
  (mode-line))

;; Notify that everything is ready!
(setf *startup-message*
      (concatenate 'string
                   "^6*Greetings ^Blogoraz^b! "
                   "Your ^BStumpWM^b session is ready..."))
