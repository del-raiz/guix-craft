;;;; swm-wpctl.lisp

;;; Copyright (C) 2023 Dmitrii Kosenkov
;;; Copyright (C) 2024 Erik P Almaraz <erikalmaraz@fastmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


(in-package :swm-wpctl)

;; formatters.
(add-screen-mode-line-formatter #\P 'modeline)
;; (add-screen-mode-line-formatter #\M 'source-modeline)

(defparameter *step* 5)

(defparameter *modeline-fmt* "%b(%v)"
  "The default value for displaying wpctl information on the modeline")

(defparameter *source-modeline-fmt* "%b(%v)"
  "The default value for displaying wpctl source information on the modeline")

(defparameter *formatters-alist*
  '((#\b  ml-bar)
    (#\v  ml-volume)))

(defparameter *wpctl-path* "/usr/bin/wpctl")
(defparameter *mixer-command* "pavucontrol")

(defparameter *default-sink-id* "@DEFAULT_AUDIO_SINK@")
(defparameter *default-source-id* "@DEFAULT_AUDIO_SOURCE@")

;; Changed to defparamter as these dynamic variables effects behavior of display state
;; of volume controls. defvar is for holding of persistent data (best practice).
;; Change indentation formating -> 80 character width preference?
(defparameter *volume-regex* (re:create-scanner "Volume: (\\d+\\.\\d+)"))
(defparameter *mute-regex* (re:create-scanner "Volume: \\d+\\.\\d+ \\[MUTED\\]"))

(defun run (args &optional (wait-output nil))
  (if wait-output
      (with-output-to-string (s)
        (sb-ext:run-program *wpctl-path* args :wait t :output s))
      (sb-ext:run-program *wpctl-path* args :wait nil)))

(defun volume-up (device-id step)
  (run (list "set-volume" device-id (format nil "~D%+" step))))

(defun volume-down (device-id step)
  (run (list "set-volume" device-id (format nil "~D%-" step))))

(defun set-volume (device-id value)
  (run (list "set-volume" device-id (format nil "~D%" value))))

;; Changed indentation to fit ~80 character width for `stumpwm-contrib'
;; submission
(defun get-volume (device-id)
  (truncate (* 100 (pf:parse-float
                    (aref (nth-value 1
                                     (re:scan-to-strings
                                      *volume-regex*
                                      (run (list "get-volume" device-id) t)))
                          0)))))

(defun get-mute (device-id)
  (and (re:scan *mute-regex*
                   (run (list "get-volume" device-id) t))
       t))

(defun unmute (device-id)
  (run (list "set-mute" device-id "0")))

(defun mute (device-id)
  (run (list "set-mute" device-id "1")))

(defun toggle-mute (device-id)
  (run (list "set-mute" device-id "toggle")))

(defun open-mixer ()
  (run-shell-command *mixer-command*))

;; May consider changing `full' parameter 5 -> 6 or to a dynamical user set variable
(defun ml-bar (volume muted)
  (concat "\["
          (bar (if muted 0 (min 100 volume)) 5 #\X #\=)
          "\]"))

(defun ml-volume (volume muted)
  (if muted "MUT" (format nil "~a\%" volume)))

;; Changed indent formatting -> 80 character width preference
(defun modeline (ml)
  (declare (ignore ml))
  (let ((ml-str (format-expand *formatters-alist*
                               *modeline-fmt*
                               (get-volume *default-sink-id*)
                               (get-mute *default-sink-id*))))
    (if (fboundp 'format-with-on-click-id) ;check in case of old stumpwm version
        (format-with-on-click-id ml-str :ml-wpctl-on-click nil)
        ml-str)))

;; Changed indent formatting -> 80 character width preference
(defun source-modeline (ml)
  (declare (ignore ml))
  (let ((ml-str (format-expand *formatters-alist*
                               *source-modeline-fmt*
                               (get-volume *default-source-id*)
                               (get-mute *default-source-id*))))
    (if (fboundp 'format-with-on-click-id) ;check in case of old stumpwm version
        (format-with-on-click-id ml-str :ml-wpctl-source-on-click nil)
        ml-str)))

(defun ml-on-click (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute *default-sink-id*))
      ((:right-button)
       (open-mixer))
      ((:wheel-up)
       (volume-up *default-sink-id* *step*))
      ((:wheel-down)
       (volume-down *default-sink-id* *step*))))
  (update-all-mode-lines))

(defun source-ml-on-click (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute *default-source-id*))
      ((:right-button)
       (open-mixer))
      ((:wheel-up)
       (volume-up *default-source-id* *step*))
      ((:wheel-down)
       (volume-down *default-source-id* *step*))))
  (update-all-mode-lines))

(when (fboundp 'register-ml-on-click-id) ;check in case of old stumpwm version
  (register-ml-on-click-id :ml-wpctl-on-click #'ml-on-click)
  (register-ml-on-click-id :ml-wpctl-source-on-click #'source-ml-on-click))

(defcommand wpctl-volume-up () ()
  "Increase the volume by N points"
  (volume-up *default-sink-id* *step*))

(defcommand wpctl-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *default-sink-id* *step*))

(defcommand wpctl-mute () ()
  "Mute"
  (mute *default-sink-id*))

(defcommand wpctl-unmute () ()
  "Unmute"
  (unmute *default-sink-id*))

(defcommand wpctl-toggle-mute () ()
  "Toggle Mute"
  (toggle-mute *default-sink-id*))

(defcommand wpctl-set-volume (value) ((:string "Volume percentage:"))
  "Set volume"
  (set-volume *default-sink-id* value))

(defcommand wpctl-source-volume-up () ()
  "Increase the volume by N points"
  (volume-up *default-source-id* *step*))

(defcommand wpctl-source-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *default-source-id* *step*))

(defcommand wpctl-source-mute () ()
  "Source mute"
  (mute *default-source-id*))

(defcommand wpctl-source-unmute () ()
  "Source unmute"
  (unmute *default-source-id*))

(defcommand wpctl-source-toggle-mute () ()
  "Toggle source Mute"
  (toggle-mute *default-source-id*))

(defcommand wpctl-source-set-volume (value) ((:string "Volume percentage:"))
  "Set source volume"
  (set-volume *default-source-id* value))
