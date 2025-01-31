;;;; end-session.asd

(defsystem "end-session"
  :description "StumpWM UI commands for shutdown, restart, and logoff."
  :author "Erik P Almaraz, Stuart Dilts"
  :license "GPLv3"
  :version "0.1.1"
  :depends-on ("stumpwm")
  :serial t
  :components ((:file "package")
               (:file "end-session")))
