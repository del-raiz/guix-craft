;;;; Keybindings for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; Commentary:
;;; TODO - Skinny down StumpWM's original Top Level keymap, don't need every option
;;;        that is present, also - it takes a while to load...

;;; References:
;;;

(in-package :stumpwm)

;;; TODO - delete *root-map* and build back with only needed keybindings
;;;        problem is, by default it is a bit polluted and has a long load time.

;;; Enable multiple keyboard layouts (English and TBD)
;; TODO - disable message for this, I don't want to see it at start up.
(load-module "kbd-layouts")
;; function immediately runs switch-keyboard-layout which provides message!
;; (kbd-layouts:keyboard-layout-list "us")

;;;  Defaults s-SPC for this command, reset & set this to prefix-key below!
(define-key *top-map* (kbd "s-k") "switch-keyboard-layout")

;;; Change the prefix key to Super-SPACE
(set-prefix-key (kbd "s-SPC"))

;;; Enable which-key
(which-key-mode)

;;; Audio/Mic Controls
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "wpctl-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "wpctl-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "wpctl-toggle-mute")
(define-key *top-map* (kbd "XF86AudioMicMute") "wpctl-source-toggle-mute")

;;; Brightness Controls
;; TODO - Create library for brightness controls (swm-brightnessctl) with moddline
;;        support.
;;      - Requires dependency brightnessctl as clarified in library name!
(define-key *top-map* (kbd "XF86MonBrightnessDown")
  "exec brightnessctl set 5%-")
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  "exec brightnessctl set +5%")

;;; Groups
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")

;;; Applications
(defvar *my-applications-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "e") "exec emacs")
    (define-key key-map (kbd "l") "exec lem")
    (define-key key-map (kbd "x") "exec xterm")
    (define-key key-map (kbd "n") "exec nyxt")
    (define-key key-map (kbd "i") "exec icecat")
    (define-key key-map (kbd "k") "exec keepassxc")
    (define-key key-map (kbd "c") "exec gnucash")
    (define-key key-map (kbd "g") "exec gimp")
    (define-key key-map (kbd "p") "exec inkscape")
    (define-key key-map (kbd "b") "exec blender")
    (define-key key-map (kbd "o") "exec obs")
    (define-key key-map (kbd "z") "exec zathura")
    key-map))
(define-key *root-map* (kbd "a") '*my-applications-keymap*)

;;; Screenshots
;; TODO - determine how to preset location for screenshots so I don't have to
;; type it in every time. Also, screenshot captures the stumpwm prompt as well...
(defvar *my-screenshot-dir* "~/desktop/screenshots/"
  "Path to custom screenshots directory.")

(defvar *my-screenshot-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "s") "screenshot")
    (define-key key-map (kbd "w") "screenshot-window")
    (define-key key-map (kbd "a") "screenshot-area")
    key-map))
(define-key *root-map* (kbd "V") '*my-screenshot-keymap*)
(define-key *top-map* (kbd "Print") '*my-screenshot-keymap*)

;;; Session Controls (end-session)
(defvar *end-session-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "q") "end-session")
    (define-key key-map (kbd "l") "lock-screen")
    ;; FIXME - set so reload configuration if possible
    ;; (define-key key-map (kbd "l")   "loadrc")
    (define-key key-map (kbd "R") "restart-hard")
    key-map))
(define-key *root-map* (kbd "q") '*end-session-keymap*)

;;; Bluetooth Controls (bluetooth)
(defvar *bluetooth-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "c") "bluetooth-connect")
    (define-key key-map (kbd "i") "bluetooth-turn-on")
    (define-key key-map (kbd "o") "bluetooth-turn-off")
    key-map))
(define-key *root-map* (kbd "B") '*bluetooth-keymap*)

;;; Slynk/Swank Server Controls
;; FIXME: slynk not loading...
(defvar *cl-server-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "w") "micros-start-server")
    (define-key key-map (kbd "x") "micros-stop-server")
    ;; (define-key key-map (kbd "y") "slynk-start-server")
    ;; (define-key key-map (kbd "z") "slynk-stop-server")
    key-map))
(define-key *root-map* (kbd "L") '*cl-server-keymap*)
