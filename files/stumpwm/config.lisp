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

;; Define Guix profiles
(defconstant +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defconstant +guix-home-path+ "/home/logoraz/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defconstant +swm-data-dir+ (concat (getenv "XDG_CACHE_HOME")
                                    "/stumpwm/"))

;;; Set PATHs: modules & data directories, etc.

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

;; Import asdf-loads
;; ref: https://www.github.com/aartaka/stumpwm-config
;; (asdf:load-systems :slynk :swank)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Initialize X11 Desktop Environment & Resources.
(load "~/.config/stumpwm/modules/auto-start.lisp")

;;; Stumpwm-contrib Packages/Libraries

;;; Load in configuration/custom files
(load "~/.config/stumpwm/modules/syntax.lisp")
(load "~/.config/stumpwm/modules/utilities.lisp")
(load "~/.config/stumpwm/modules/colors.lisp")
(load "~/.config/stumpwm/modules/theme.lisp")
(load "~/.config/stumpwm/modules/frames.lisp")
(load "~/.config/stumpwm/modules/keybindings.lisp")
(load "~/.config/stumpwm/modules/modeline.lisp")

;; Start the mode line
;; TODO: Put in modeline module?
(when *initializing*
  (mode-line))

;; Notify that everything is ready!
(setf *startup-message*
      (concatenate 'string
                   "^6*Welcome ^Blogoraz^b. "
                   "Your ^BStumpWM^b session is ready!"))
