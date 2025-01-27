;;;; Nyxt Utilities Module

;;; Commentary:
;;;

;;; References
;;; 1.
;;; 2.

(in-package :nyxt-user)

(require :micros)

(defvar *micros-port* 4006
  "Default Common Lisp server port for Nyxt")

;; Lem connection to Nyxt

;; (define-command start-micros (&optional (micros-port *micros-port*))
;;   "Start a Micros server enabling connecting to Lem via SLIME."
;;   (micros:create-server :port micros-port :dont-close t)
;;   (echo "Micros server started at port ~a" micros-port))

;; (define-command stop-micros (&optional (micros-port *micros-port*))
;;   "Stop current Micros server."
;;   (micros:stop-server micros-port)
;;   (echo "Closing Micros server at port ~a" micros-port))
