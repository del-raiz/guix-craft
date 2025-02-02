;;;; Nyxt Theme Module

;;; Commentary:
;;;

;;; References:
;;; 1. Invader Theme:
;;;    - git clone https://github.com/atlas-engineer/invader
;;;      or
;;;    - wget nyxt-browser.com/static/release/invader.tar.gz
;;; 2. Sourced Saved in ~/.local/share/nyxt/extensions/invader

;;; Only for nxyt-3 or greater
(nyxt:define-package :nx-invader-2)

(in-package :nx-invader-2)

;;; Nord Color Palette
(defvar raz-nord0 "#2e3440")   ; 'Black'
(defvar raz-nord1 "#3b4252")   ; "Dark Gray'
(defvar raz-nord2 "#434c5e")   ; 'Medium Gray'
(defvar raz-nord3 "#4c566a")   ; 'Gray'
(defvar raz-nord4 "#d8dee9")   ; 'Light grey'
(defvar raz-nord5 "#e5e9f0")   ; 'Off-white'
(defvar raz-nord6 "#eceff4")   ; 'White'
(defvar raz-nord7 "#8fbcbb")   ; 'Blue/Green'
(defvar raz-nord8 "#88c0d0")   ; 'Teal'
(defvar raz-nord9 "#81a1c1")   ; 'Blue/Gray'
(defvar raz-nord10 "#5e81ac")  ; 'Blue'
(defvar raz-nord11 "#bf616a")  ; 'Red'
(defvar raz-nord12 "#d08770")  ; 'Orange'
(defvar raz-nord13 "#ebcb8b")  ; 'Yellow'
(defvar raz-nord14 "#a3be8c")  ; 'Green'
(defvar raz-nord15 "#b48ead")  ; 'Purple'


;;; Invader Theme
(defvar invader-theme-2
  (make-instance 'theme:theme
                 :dark-p t
                 :background-color- "#303240"
                 :background-color "#282A36"
                 :background-color+ "#1E2029"
                 :on-background-color "#F7FBFC"

                 :primary-color- "#679BCF"
                 :primary-color "#789FE8"
                 :primary-color+ "#7FABD7"
                 :on-primary-color "#0C0C0D"

                 :secondary-color- "#44475A"
                 :secondary-color "#44475A"
                 :secondary-color+ "#535A6E"
                 :on-secondary-color "#F7FBFC"

                 :action-color- "#6BE194"
                 :action-color "#4FDB71"
                 :action-color+ "#27BF4C"
                 :on-action-color "#0C0C0D"

                 :success-color- "#86D58E"
                 :success-color "#8AEA92"
                 :success-color+ "#71FE7D"
                 :on-success-color "#0C0C0D"

                 :highlight-color- "#EA43DD"
                 :highlight-color "#F45DE8"
                 :highlight-color+ "#FC83F2"
                 :on-highlight-color "#0C0C0D"

                 :warning-color- "#FCA904"
                 :warning-color "#FCBA04"
                 :warning-color+ "#FFD152"
                 :on-warning-color "#0C0C0D"

                 :codeblock-color- "#3C5FAA"
                 :codeblock-color "#355496"
                 :codeblock-color+ "#2D4880"
                 :on-codeblock-color "#F7FBFC"))

(define-configuration :browser
    ((theme invader-theme-2)))

(define-configuration :status-buffer
    ((style (str:concat %slot-value%
                        (theme:themed-css (theme *browser*))))))
