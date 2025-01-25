(define-module (config home services emacs)
  #:use-module (gnu)                        ; -
  #:use-module (gnu packages emacs)         ;-> emacs
  #:use-module (gnu packages emacs-xyz)     ;-> all emacs-* packages
  #:use-module (gnu packages guile)         ;-> guile-next
  #:use-module (gnu packages guile-xyz)     ;-> guile-ares-rs
  #:use-module (gnu packages mail)          ;-> isync, msmtp, mu
  #:use-module (gnu services configuration) ; -
  #:use-module (gnu home services)          ; -
  #:use-module (guix gexp)                  ; -
  #:use-module (guix transformations)       ; -

  #:export (home-emacs-config-service-type))


(define (home-emacs-config-profile-service config)
  (list emacs
        emacs-diminish ;;|--> gnu packages emacs-xyz
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
        isync
        msmtp
        mu

        ;; IRC & Tools
        emacs-erc-hl-nicks
        emacs-erc-image
        emacs-emojify
        emacs-0x0 ;; a pastebin integration tool

        ;; Development Packages
        emacs-magit
        emacs-vterm
        emacs-paredit
        emacs-arei
        emacs-guix ;; comes loaded with geiser and geiser-guile etc...
        emacs-macrostep
        emacs-sly
        emacs-arei ;; Experimental alternative to geiser

        ;; Guile Scheme Emacs Integration
        guile-next                      ;doesn't work with g-golf
        guile-ares-rs))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)))
                (default-value #f)))
