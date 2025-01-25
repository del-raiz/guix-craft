;;;; auto-start.lisp --> Start X11 environment for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)

;;; X11 Settings

;;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")
;; xrandr stuff would go here too?

;;; Set pointer/cursor paths & defaults -> theme set in .Xresources
;; Ref |--> https://github.com/ful1e5/XCursor-pro
(run-shell-command "xsetroot -xcf ~/.icons/XCursor-Pro-Dark/cursors/left_ptr 22")

;;; Turn off system bell & screen-saver control
(run-shell-command "xset b off")
(run-shell-command "xset s off")

;;; Disable Trackpad (set here as default)
;; TODO: Establish as a command so the user can toggle on/off via StumpWM
;;       |---> Place in `syntax.lisp'
;; FIXME: The ID changes after a system reconfigure --> need a way to resolve this
;;        without having to update the code ever reconfigure...
;; To list inputs -> xinput --list --> needed to establish <device> aka "id"
;; xinput list-props 10 | head -n5 --> needed to establish <propert>
(run-shell-command "xinput set-prop 12 185 0")

;;; UI Settings

;;; Set Wallpaper
(run-shell-command "feh --bg-scale  ~/Pictures/wallpapers/sunset-mountain.jpg")

;;; Enable screen compositing
(run-shell-command "picom")

;;; Enable screen locking on suspend
(run-shell-command "xss-lock -- slock")

;;; Additional Xorg resources
(run-shell-command "xrdb -merge ~/.Xresources")
