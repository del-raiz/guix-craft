;; Guix manifest to display all Emacs packages used
;; this is included in my home emacs-guile.scm profil service

(use-modules (guix packages)
             (gnu packages emacs-xyz)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages version-control))       ;for libadwaita

(packages->manifest
  (list emacs-diminish       ;;|--> gnu packages emacs-xyz
        emacs-delight
        emacs-nord-theme
        emacs-doom-themes
        emacs-nerd-icons
        emacs-doom-modeline
        emacs-ligature
        emacs-no-littering
        emacs-ws-butler
        emacs-undo-tree

        emacs-visual-fill-column
        emacs-ace-window
        emacs-mct
        emacs-orderless
        emacs-corfu
        emacs-marginalia
        emacs-beframe
        emacs-denote
        emacs-consult-denote

        ;; Mail & Org
        ;;https://packages.guix.gnu.org/packages/emacs-pinentry/0.1-1.dcc9ba0/
        emacs-pinentry
        emacs-mbsync
        emacs-org-superstar
        emacs-org-appear

        ;; IRC
        emacs-erc-hl-nicks
        emacs-erc-image
        emacs-emojify

        ;; Development Packages
        emacs-magit
        emacs-vterm
        emacs-paredit
        emacs-arei
        emacs-guix ;; comes loaded with geiser and geiser-guile etc...
        emacs-macrostep
        emacs-sly
        emacs-arei ;; Experimental alternative to geiser

        ;; Utilities
        git
        (list git "send-email")

        ;; Guile Scheme Emacs Integration
        ;; guile-3.0      ;not needed since I live in Guix SD!
        guile-next     ;doesn't work with g-golf
        guile-ares-rs
        guile-colorized
        ;; Common Lisp Integration
        sbcl
        sbcl-slynk)
