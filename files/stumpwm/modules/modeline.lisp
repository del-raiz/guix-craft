;;;; modeline.lisp --> Modeline Configuration for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;; 1. StumpWM Contrib: https://github.com/stumpwm/stumpwm-contrib
;;; 2. battery-portible contrib
;;;    https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable
;;; 3. mem module contrib
;;;    https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem
;;;    Then you can use “%M” and/or “%N in your mode line format.
;;; 4. cpu module contrib
;;; From: https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu
;;; Then you can use %C in your mode line format:

(in-package :stumpwm)

;; The timeout of the modeline indicates how often it refreshes in seconds.
(setf *mode-line-timeout* 2)

;; Formatting options
(setf *time-modeline-string* "%F %H:%M")

;; Let’s also indicate how the groupname is displayed.
(setf *group-format* "%t")

;; The window format should display first its window number, then title -
;; limited to 30 characters.
(setf *window-format* " %n: %30t ")

;; Set mode-line colors
(setf *mode-line-background-color* logoraz-nord0
      *mode-line-foreground-color* logoraz-nord4)

(setf *mode-line-border-color* logoraz-nord3
      *mode-line-border-width* 1)

;;; Modeline Packages (stumpwm-contrib)
(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")
(load-module "wifi")
 
;;; Custom Module Settings
;; f0 = font-hack, f1 = font-jetbrains-mono
(setf cpu::*cpu-modeline-fmt*        "%c %t"
      cpu::*cpu-usage-modeline-fmt*  "^f1⊠^f0 ^[~A~0D%^]"
      mem::*mem-modeline-fmt*        "≡ %a%p"
      *hidden-window-color*          "^**"
      *mode-line-highlight-template* "«~A»"
      wifi::*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")


;;; Modeline Formatter
;;TODO: change format so groups are blue...
(defvar *mode-line-formatter-list*
  '(("%g")  ;; Groups
    ("%W")  ;; Windows
    ("^>")  ;; StumpWM modeline seperator
    ("%P")  ;; Audio info
    ("%C")  ;; CPU usage
    ("%M")  ;; Memory usage
    ("%I")  ;; Wifi status
    ("%B")  ;; Battery info
    ("%d")) ;; Date/Time
  "List of formatters for the modeline.")

(defun generate-modeline (elements &optional not-invertedp rightp)
  "Generate a modeline for StumpWM.
ELEMENTS should be a list of `cons'es which `first' is the modeline
formatter or the shell command to run, and their `rest' is either nil
when the `first' is a formatter and t when it is a shell command."
  (when elements
    (cons (format nil " ^[~A^]^(:bg \"~A\") "
                  (format nil "^(:fg \"~A\")^(:bg \"~A\")~A"
                          (if (xor not-invertedp rightp)
                              logoraz-nord0 logoraz-nord2)
                          (if (xor not-invertedp rightp)
                              logoraz-nord2 logoraz-nord0)
                          (if rightp "" ""))
                  (if not-invertedp logoraz-nord2 logoraz-nord0))
          (let* ((current-element (first elements))
                 (formatter       (first current-element))
                 (commandp        (rest current-element)))
            (cons (if commandp
                      `(:eval (run-shell-command ,formatter t))
                    (format nil "~A" formatter))
                  (generate-modeline (rest elements)
                                     (not not-invertedp)
                                     (if (string= "^>" (first (first elements)))
                                         t rightp)))))))

(defcommand reload-modeline () ()
  "Reload modeline."
  (sb-thread:make-thread
   (lambda ()
     (setf *screen-mode-line-format*
           (rest (generate-modeline *mode-line-formatter-list*))))))

(reload-modeline)
