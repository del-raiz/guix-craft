;;;; Start X11 environment for StumpWM

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
(run-shell-command "xinput set-prop 12 185 0")

;;; UI Settings

;;; Set Wallpaper
(run-shell-command "feh --bg-scale  ~/Pictures/wallpapers/desktop-bg.jpg")

;;; Enable screen locking on suspend
(run-shell-command "xss-lock -- slock")

;;; Additional Xorg resources
(run-shell-command "xrdb -merge ~/.Xresources")

;;; Enable screen compositing
(run-shell-command "picom --backend xrender")
