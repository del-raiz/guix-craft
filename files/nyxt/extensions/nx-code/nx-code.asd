;;; Experimental

(defsystem nx-code
  :description "Modern Emacs-like Editor/IDE for Nyxt."
  :author "Erik P Almaraz"
  :license "BSD 3-clause"
  :version "0.0.1"
  :depends-on ("nyxt"
               "local-time")
  :serial t
  :components ((:file "nx-code")))
