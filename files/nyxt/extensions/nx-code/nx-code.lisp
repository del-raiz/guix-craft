;;; Only for nxyt-3 or greater
(nyxt:define-package :nx-code)

(in-package :nx-code)


(defvar *locutus* "nx-code"
  "First contact from nx-code")

(define-command-global nx-code (&optional (locutus *locutus*))
  "Test command."
  (let* ((current-time (local-time:now))
         (current-day-as-string (aref local-time:+day-names+
                                      (local-time:timestamp-day-of-week current-time))))
    (echo "First Contact: ~a ~a." locutus current-day-as-string)))
