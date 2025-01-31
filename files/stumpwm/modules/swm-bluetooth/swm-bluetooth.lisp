;;;; swm-bluetooth.lisp

;;; Copyright (C) 2024  Erik P Almaraz <erikalmaraz@fastmail.com>
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

;;; Commentary:
;;;

;;; References:
;;; 1. https://config.phundrak.com/stumpwm#bluetooth
;;; 2. TBD


(in-package :swm-bluetooth)

(defvar *bluetooth-command* "bluetoothctl"
  "Base command for interacting with bluetooth.")

(defun format-output (value)
  "Format output string VALUE to remove shell formatting, returning just the state identifier."
  (let* ((regexp "\\[(.*?)\\]") ;; captures [CHG], [DEL], etc.
         (filter "[A-Z]+") ;; filters to replace content w/o brackets.
         (match (re:scan-to-strings regexp value))
         (content (re:scan-to-strings filter match)))
    (re:regex-replace-all regexp value (concat content " -"))))

(defun bluetooth-message (&rest output)
  "Bluetooth message formatting."
  (message (format-output (format nil "^2Bluetooth:^7 ~{~a~^ ~}" output))))

(defun bluetooth-make-command (&rest args)
  "Make bluetooth command."
  (format nil "~a ~{~a~^ ~}" *bluetooth-command* args))

(defmacro bluetooth-command (&rest args)
  "Bluetooth command macro."
  `(run-shell-command (bluetooth-make-command ,@args) t))

(defmacro bluetooth-message-command (&rest args)
  "Bluetooth message macro for commands."
  `(bluetooth-message (bluetooth-command ,@args)))

;; TODO - Research more on `defstruct' data structures.
(defstruct (bluetooth-device
            (:constructor make-bluetooth-device (&key (address "") (name nil)))
            (:constructor make-bluetooth-device-from-command
                (&key (raw-name "")
                 &aux (address (second (re:split " " raw-name)))
                   (full-name (format nil "~{~a~^ ~}"
                                      (rest (rest (re:split " " raw-name))))))))
  "Bluetooth device data structure for establishing various methods of conection."
  address
  (full-name (progn (format nil "~{~a~^ ~}" name))))

(defun bluetooth-get-devices ()
  "Documentation"
  (let ((literal-devices (bluetooth-command "devices")))
    (mapcar (lambda (device)
              (make-bluetooth-device-from-command :raw-name device))
            (re:split "\\n" literal-devices))))

(defun bluetooth-connect-device (device)
  "Connect to a bluetooth device."
  (progn
    (bluetooth-turn-on)
    (cond ((bluetooth-device-p device) ;; it is a bluetooth-device structure
           (bluetooth-message-command "connect"
                                      (bluetooth-device-address device)))
          ((stringp device) ;; assume it is a MAC address
           (bluetooth-message-command "connect" device))
          (t (message (format nil "Cannot work with device ~a" device))))))


;;; StumpWM Interface
(defcommand bluetooth-connect () ()
            "Connect to an established device."
            (sb-thread:make-thread
             (lambda ()
               (let* ((devices (bluetooth-get-devices))
                      (choice  (second (select-from-menu
                                        (current-screen)
                                        (mapcar (lambda (device)
                                                  `(,(bluetooth-device-full-name device)
                                                    ,device))
                                                devices)))))
                 (bluetooth-connect-device choice)))))

(defcommand bluetooth-turn-on () ()
  "Turn on bluetooth."
  (bluetooth-message-command "power" "on"))

(defcommand bluetooth-turn-off () ()
  "Turn off bluetooth."
  (bluetooth-message-command "power" "off"))
