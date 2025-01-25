(define-module (config home services local-files)
  #:use-module (ice-9 optargs)              ; -
  #:use-module (gnu home)                   ; -
  #:use-module (gnu home services)          ; -
  #:use-module (guix gexp)                  ; -
  #:use-module (gnu home services dotfiles) ; -
  #:use-module (config home services home-impure-symlinks)

  #:export (home-local-files-service-type))

;; Edit setting the Home User
(define %user-name "logoraz")

(define *home-path* "/home/logoraz/dotfiles/")

;; (define %source (string-append "/home"
;;                                "/" %user-name
;;                                "/dotfiles"))

;; (define (home-file dir filename)
;;   "Resolve local config file."
;;   (local-file (string-append
;;                %source "/"
;;                dir "/"
;;                filename)
;;               #:recursive? #t))


(define (home-local-files-gexp-service config)
  `( ;; guix Configuration Scaffolding
    (".config/guix/channels.scm"
     ,(string-append
       *home-path*
       "config/system/channels.scm"))

    ;; StumpWM XDG Configuration Scaffolding
    (".config/stumpwm/config"
     ,(string-append
       *home-path*
       "files/stumpwm/config.lisp"))

    (".config/stumpwm/libraries"
     ,(string-append
       *home-path*
       "files/stumpwm/libraries"))

    (".config/stumpwm/modules"
     ,(string-append
       *home-path*
       "files/stumpwm/modules"))

    ;; Xorg Configuration Scaffolding
    (".Xdefaults"
     ,(string-append
       *home-path*
       "files/xorg/dot-Xdefaults"))

    (".Xresources"
     ,(string-append
       *home-path*
       "files/xorg/dot-Xresources"))

    (".icons"
     ,(string-append
       *home-path*
       "files/xorg/dot-icons"))

    (".config/xorg/start-xterm.sh"
     ,(string-append
       *home-path*
       "files/xorg/start-xterm.sh"))

    ;; Emacs Configuration Scaffolding
    (".config/emacs"
     ,(string-append
       *home-path*
       "files/emacs"))

    ;; Lem Configuration
    (".config/lem"
     ,(string-append
       *home-path*
       "files/lem"))

    ;; Nyxt Configuration Scaffolding
    (".config/nyxt"
     ,(string-append
       *home-path*
       "files/nyxt"))

    (".local/share/nyxt/extensions"
     ,(string-append
       *home-path*
       "files/nyxt/extensions"))))

(define home-local-files-service-type
  (service-type (name 'home-xdg-files)
                (description "Service for setting up local files.")
                (extensions
                 (list (service-extension
                        home-impure-symlinks-service-type
                        home-local-files-gexp-service)))
                (default-value #f)))
