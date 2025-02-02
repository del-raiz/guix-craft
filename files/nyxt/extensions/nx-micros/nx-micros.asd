(defsystem "nx-micros"
  :author "Erik P Almaraz"
  :license "BSD 3-clause"
  :version "0.0.1"
  :description "Connect Nyxt to Lem via Micros/Swank server."
  :serial t
  :depends-on ("nyxt"
               "micros")
  :components ((:file "nx-micros")))
