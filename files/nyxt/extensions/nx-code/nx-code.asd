;;; Experimental

(defsystem "nx-code"
  :description "Modern Emacs-like Editor/IDE for Nyxt."
  :author "Erik P Almaraz"
  :license "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on ("nyxt"
               "local-time")
  :components ((:file "nx-code")))
