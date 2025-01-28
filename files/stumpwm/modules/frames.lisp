;;;; StumpWM Screen Configuration:  Frames & Windows

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)

;;; Window Groups & Placement
(when *initializing*
  (grename "[HOME]")
  (gnewbg  "[DEV]")
  (gnewbg  "[WWW]")
  (gnewbg  "[ETC]")
  (gnewbg  "[PRIV]"))

(clear-window-placement-rules)

(define-frame-preference "[HOME]" (nil t t :class "Tiling"))
(define-frame-preference "[DEV]"  (nil t t :class "Tiling"))
(define-frame-preference "[WWW]"  (nil t t :class "Tiling"))
(define-frame-preference "[ETC]"  (nil t t :class "Tiling"))
(define-frame-preference "[PRIV]" (nil t t :class "Tiling"))

(setf *dynamic-group-master-split-ratio* 1/2)

;;; X-window settings & stylization via gaps
;; Tell stumpwm to not honor application window size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Window Gaps
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

;; Window Stylization Parameters
(setf *maxsize-border-width       2
      *normal-border-width*       2
      *float-window-border*       2
      *float-window-title-height* 15
      *window-border-style*       :thick
      *window-format*             "%n:%t")

;; (set-win-bg-color        logoraz-nord2)
(set-focus-color         logoraz-nord9)
(set-unfocus-color       logoraz-nord3)
(set-float-focus-color   logoraz-nord9)
(set-float-unfocus-color logoraz-nord3)

;;; Message and Input Windows Stylization Parameters
(set-border-color logoraz-nord9)
(set-fg-color logoraz-nord4)
(set-bg-color logoraz-nord0)

(setf *key-seq-color* "^6")
(setf *which-key-format* (concat *key-seq-color* "*~5a^n ~a"))

(setf *input-window-gravity*     :top-right
      *message-window-y-margin*  30
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity    :top-right)


;;; Mouse focus/float window settings
(setf *mouse-focus-policy* :click ; Mouse click should focus the window
      *float-window-modifier* :SUPER) ; Set super key to move floating windows

;;; Navigate between windows from all workspaces
(load-module "globalwindows")
