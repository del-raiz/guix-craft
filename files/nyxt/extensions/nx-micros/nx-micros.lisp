;;; Only for nxyt-3 or greater
(nyxt:define-package :nx-micros)

(in-package :nx-micros)


(defvar *micros-port* 4006
  "Default Common Lisp server port for Nyxt")

(define-command-global start-micros (&optional (micros-port *micros-port*))
    "Start a Micros server enabling connecting to Lem via SLIME."
  (micros:create-server :port micros-port :dont-close t)
  (echo "Micros server started at port ~a" micros-port))

(define-command-global stop-micros (&optional (micros-port *micros-port*))
    "Stop current Micros server."
  (micros:stop-server micros-port)
  (echo "Closing Micros server at port ~a" micros-port))
