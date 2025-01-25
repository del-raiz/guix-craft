;;;; notes.lisp --> lem

;;; Problem Statement:
;;  |--> When connecting to a running micros instance (outside of Lem) on a new server :port
;;       repl is successfully generated, and connected to designated :port, however, repl
;;       message is not clearly verbose about what connection was established. And sometimes
;;       has a comletely inaccurate message:
;;  |--> Message
;;      Welcome to the REPL!
;;
;;      The current REPL is running in the same process as the editor.
;;      If you don't need to hack the editor,
;;      please start a new process with `M-x slime`.
;;
;;  |--> Try to modify code to state what connection (i.e. port was connected to)


;;; lem/extensions/lisp-mode/lisp-mode.lisp
(define-command slime-connect (hostname port &optional (start-repl t))
    ((:splice
      (list (prompt-for-string "Hostname: " :initial-value *localhost*)
            (parse-integer
             (prompt-for-string "Port: "
                                :initial-value (princ-to-string *default-port*))))))
  (let ((connection (connect-to-micros hostname port)))
    (when start-repl (start-lisp-repl))
    (connected-slime-message connection)))


;; Lem uses xdg-open
;; Not sure why xdg-open opens icecate even when nyxt is set as the defaul browser.
;; in $BROWSER env...
;; /gnu/store/hzkdz5cagnp1a102l65cp8kp0d4mqvi6-xdg-utils-1.1.3/bin/.xdg-open-real
;; /gnu/store/hzkdz5cagnp1a102l65cp8kp0d4mqvi6-xdg-utils-1.1.3/bin/xdg-open
;; /gnu/store/x47i4yafqxdav838aykda9c2hhhn9sa4-bash-minimal-5.1.16/bin/bash
