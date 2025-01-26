(define-module (config home services environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:export (home-env-vars-configuration-service-type))


;; Edit setting the Home User
(define %user-name "logoraz")

(define %gtk2-rc ".guix-home/profile/share/themes/Adwaita-dark/gtk-2.0/gtkrc")

(define (home-path directory)
  (string-append
   "/home"
   "/" %user-name "/"
   directory))

;; borrowed from https://codeberg.org/daviwil/dotfiles/daviwil/systems/common.scm
(define (home-env-vars-config-gexp config)
  `( ;; Sort hidden (dot) files first in ls listings
    ("LC_COLLATE" . "C")

    ;; Set Default editor
    ("EDITOR" . "lem")
    ;; ("VISUAL" . "lem")

    ;; Set quotebrowser as the default
    ("BROWSER" . "nyxt")

    ;; Set GnuPG Config Dir env
    ("GNUPGHOME" . "$XDG_CONFIG_HOME/gnupg")

    ;; GTK Theme
    ("GTK_THEME" . "Adwaita:dark")

    ;; Set XDG environment variables
    ("XDG_SESSION_TYPE" . "x11")
    ("XDG_SESSION_DESKOP" . "stumpwm")
    ("XDG_CURRENT_DESKTOP" . "stumpwm")
    ("XDG_DOWNLOAD_DIR" . ,(home-path "Downloads"))
    ("XDG_PICTURES_DIR" . ,(home-path "Pictures/Screenshots"))))


(define home-env-vars-configuration-service-type
  (service-type (name 'home-profile-env-vars-service)
                (description "Service for setting up profile env vars.")
                (extensions
                 (list (service-extension
                        home-environment-variables-service-type
                        home-env-vars-config-gexp)))
                (default-value #f)))
