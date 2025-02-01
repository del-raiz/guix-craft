(define-module (config home services mutable-files)
  #:use-module (ice-9 optargs)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu home services dotfiles)
  #:use-module (config home services home-impure-symlinks)

  #:export (home-mutable-files-service-type))


(define *home-path* "/home/logoraz/dotfiles/")

(define (home-mutable-files-gexp-service config)
  `(;; guix Configuration Scaffolding
    (".config/guix/channels.scm"
     ,(string-append
       *home-path*
       "config/system/channels.scm"))

    ;; StumpWM XDG Configuration Scaffolding
    (".config/stumpwm/config"
     ,(string-append
       *home-path*
       "files/stumpwm/config.lisp"))

    (".config/stumpwm/modules"
     ,(string-append
       *home-path*
       "files/stumpwm/modules"))

    (".config/stumpwm/source"
     ,(string-append
       *home-path*
       "files/stumpwm/source"))

    ;; Xorg/XDG Configuration Scaffolding
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

    (".local/share/fonts/SymbolsNerdFontMono-Regular.ttf"
     ,(string-append
       *home-path*
       "files/xorg/fonts/SymbolsNerdFontMono-Regular.ttf"))

    (".local/share/fonts/FiraCodeNerdFontMono-Regular.ttf"
     ,(string-append
       *home-path*
       "files/xorg/fonts/FiraCodeNerdFontMono-Regular.ttf"))

    (".local/share/fonts/FiraCodeNerdFontMono-Bold.ttf"
     ,(string-append
       *home-path*
       "files/xorg/fonts/FiraCodeNerdFontMono-Bold.ttf"))

    ;; Emacs Configuration Scaffolding
    (".config/emacs"
     ,(string-append
       *home-path*
       "files/emacs"))

    ;; Lem Configuration Scaffolding
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

(define home-mutable-files-service-type
  (service-type (name 'home-mutable-files)
                (description "Service for mutable local file symlinking.")
                (extensions
                 (list (service-extension
                        home-impure-symlinks-service-type
                        home-mutable-files-gexp-service)))
                (default-value #f)))
