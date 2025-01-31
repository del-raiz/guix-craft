;;;; swm-screenshot.asd

(defsystem "swm-screenshot"
  :serial t
  :description "Takes screenshots and stores them as png files"
  :author "Erik P Almaraz, Michael Filonenko"
  :license "GPLv3"
  :depends-on ("stumpwm" "clx" "zpng" "local-time")
  :components ((:file "package")
               (:file "swm-screenshot")))
