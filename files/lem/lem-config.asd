;;;; lem-config.asd

(asdf/parse-defsystem:defsystem "lem-config"
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.0.1"
  :description "Lem Configuration."
  :serial t
  :depends-on ()
  :components ((:module "source"
                :components ((:file "paredit")
                             (:file "appearance")
                             (:file "utilities")
                             (:file "misc")
                             (:file "completions")
                             (:file "keybindings")
                             (:file "file-prompt")
                             (:file "time-stamp")))))
