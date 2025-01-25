(define-module (config home services xdg-files)
  #:use-module (ice-9 optargs)              ; -
  #:use-module (gnu home)                   ; -
  #:use-module (gnu home services)          ; -
  #:use-module (guix gexp)                  ; -
  #:use-module (gnu home services dotfiles) ; -

  #:export (home-xdg-local-files-service-type))

;; Edit setting the Home User
(define %user-name "logoraz")

(define %source (string-append "/home"
                               "/" %user-name
                               "/dotfiles"))

(define (home-file dir filename)
  "Resolve local config file."
  (local-file (string-append
               %source "/"
               dir "/"
               filename)
              #:recursive? #t))

(define (home-xdg-local-files-gexp-service config)
  `(;;TODO: prep for GNU guile next relase to store these in XDG_CONFIG_HOME...
    ("guile/guile-rc.scm"
     ,(home-file "files/guile" "guile-rc.scm"))

    ;; GTK configuration --> needed for mouse theme on GTK applications
    ("gtk-3.0/settings.ini"
     ,(home-file "files/gtk-3.0" "settings.ini"))

    ;; GnuPG Configuration
    ("gnupg/gpg-agent.conf"
     ,(home-file "files/gnupg" "gpg-agent.conf"))

    ;; mbsync configuration (for mu/mu4e)
    ("isyncrc"
     ,(home-file "files/isync" "isyncrc"))

    ;; msmtp configuration (for mu/mu4e)
    ("msmtp/config"
     ,(home-file "files/msmtp" "config"))

    ;; Terminal configurations
    ("vterm/emacs-vterm-bash.sh"
     ,(home-file "files/vterm" "emacs-vterm-bash.sh"))

    ;; Zathura Configuration File
    ("zathura/zathurarc"
     ,(home-file "files/zathura" "zathurarc"))))

(define home-xdg-local-files-service-type
  (service-type (name 'home-xdg-files)
                (description "Service for setting up XDG local files.")
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        home-xdg-local-files-gexp-service)))
                (default-value #f)))
