;;;; swm-wpctl.asd

(defsystem "swm-wpctl"
  :description "PipeWire (WirePlumber) volume and microphone control module for StumpWM"
  :author "Erik P Almaraz, Dmitrii Kosenkov"
  :license  "GPLv3"
  :version "0.1.1"
  :serial t
  :depends-on ("stumpwm" "parse-float" "cl-ppcre")
  :components ((:file "package")
               (:file "swm-wpctl")))
