;;;; swm-bluetooth.asd

(defsystem "swm-bluetooth"
  :description "Provides simple interface to connect to Bluetooth."
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.1.1"
  :serial t
  :depends-on ("stumpwm" "cl-ppcre")
  :components ((:file "package")
               (:file "swm-bluetooth")))
