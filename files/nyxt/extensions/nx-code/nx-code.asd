;;; Experimental

(defsystem nx-code
  :author "Erik P Almaraz"
  :license "BSD 3-clause"
  :version "0.0.1"
  :description "Modern Emacs-like Editor/IDE for Nyxt."
  :depends-on ("nyxt"
               "local-time")
  :serial t
  :components ((:file "nx-code")))
