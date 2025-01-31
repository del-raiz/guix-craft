;;;; Utilities for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;; TODO: determine a better way to load in modules/packages

;;; References:
;;;

(in-package :stumpwm)

;;; Pipewire/Wirepluber Audio Controls for StumpWM
;; TODO: Refactor -> move to modeline file.
(add-to-load-path #p"~/.config/stumpwm/modules/swm-wpctl/")
(load-module "swm-wpctl")
(setf swm-wpctl:*wpctl-path* "/run/current-system/profile/bin/wpctl")
(setf swm-wpctl:*mixer-command* "playerctl")

;;; Simple Bluetooth Controls for StumpWM
;; TODO: Add modeline display
(add-to-load-path #p"~/.config/stumpwm/modules/swm-bluetooth/")
(load-module "swm-bluetooth")

;;; Screenshots via Common Lisp -> removed scrot!
;; Modified stumpwwm-contrib package screenshot
(add-to-load-path #p"~/.config/stumpwm/modules/swm-screenshot/")
(load-module "swm-screenshot")

;;; Stumpwm-contrib packages not available in Guix
;; `end-session' - Provides session control commands, i.e. shutdown, restart,
;; and logoff for StumpWM.
(add-to-load-path #p"~/.config/stumpwm/modules/end-session/")
(load-module "end-session")
;; Use loginctl instead of the default systemctl
(setf end-session:*end-session-command* "loginctl")

;;; Screensaver command for slock
(defvar *screenlock-command* "slock"
  "Set screenlock command executable, default is slock.")

(defcommand lock-screen () ()
  "Screenlock command using slock - bound in keybindings under end-session map."
  (run-shell-command *screenlock-command*))


;;; Experimental stumpwm-contrib packages (to trial)
;; requires sbcl-xml-emitter and sbcl-dbus -> failing w/ error on sb-rotate-byte...
;; (load-module "notify")
;; (notify:notify-server-toggle)
