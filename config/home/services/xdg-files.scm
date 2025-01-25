(define-module (config home services xdg-files)
  #:use-module (ice-9 optargs)              ; -
  #:use-module (gnu home)                   ; -
  #:use-module (gnu home services)          ; -
  #:use-module (guix gexp)                  ; -
  #:use-module (gnu home services dotfiles) ; -

  #:export (home-xdg-local-files-service-type))

;; Edit setting the Home User
(define %user-name "loraz")

(define %source (string-append "/home"
                               "/" %user-name
                               "/.guixos-sway"))

(define (home-file dir filename)
  "Resolve local config file."
  (local-file (string-append
               %source "/"
               dir "/"
               filename)
              #:recursive? #t))

(define (home-xdg-local-files-gexp-service config)
  `(;; Guix Configuration Channels
    ("guix/channels.scm"
     ,(home-file "config/system" "channels.scm"))

    ;;TODO: prep for GNU guile next relase to store these in XDG_CONFIG_HOME...
    ("guile/guile-rc.scm"
     ,(home-file "files/guile" "guile-rc.scm"))

    ;; Sway configuration files
    ("sway/config"
     ,(home-file "files/sway" "config"))

    ("sway/bin/swaybar-status.sh"
     ,(home-file "files/sway/bin" "swaybar-status.sh"))

    ("sway/bin/toggle-display.sh"
     ,(home-file "files/sway/bin" "toggle-display.sh"))

    ;; GTK configuration --> needed for mouse theme on GTK applications
    ("gtk-3.0/settings.ini"
     ,(home-file "files/gtk-3.0" "settings.ini"))

    ;; Mako configuration
    ("mako/config"
     ,(home-file "files/mako" "config"))

    ;; Fuzzel configuration
    ("fuzzel/fuzzel.ini"
     ,(home-file "files/fuzzel" "fuzzel.ini"))

    ;; wlogout configuration
    ("wlogout/layout"
     ,(home-file "files/wlogout" "layout"))

    ("wlogout/style.css"
     ,(home-file "files/wlogout" "style.css"))

    ("wlogout/icons/lock.svg"
     ,(home-file "files/wlogout/icons" "lock.svg"))

    ("wlogout/icons/logout.svg"
     ,(home-file "files/wlogout/icons" "logout.svg"))

    ("wlogout/icons/reboot.svg"
     ,(home-file "files/wlogout/icons" "reboot.svg"))

    ("wlogout/icons/shutdown.svg"
     ,(home-file "files/wlogout/icons" "shutdown.svg"))

    ("wlogout/icons/suspend.svg"
     ,(home-file "files/wlogout/icons" "suspend.svg"))

    ("wlogout/icons/hibernate.svg"
     ,(home-file "files/wlogout/icons" "hibernate.svg"))

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
    ("foot/foot.ini"
     ,(home-file "files/foot" "foot.ini"))

    ("vterm/emacs-vterm-bash.sh"
     ,(home-file "files/vterm" "emacs-vterm-bash.sh"))

    ;; Qutebrowser Configuration Files
    ("qutebrowser/config.py"
     ,(home-file "files/qutebrowser" "config.py"))

    ("qutebrowser/quteconfig.py"
     ,(home-file "files/qutebrowser" "quteconfig.py"))

    ("qutebrowser/qute-keepassxc"
     ,(home-file "files/qutebrowser" "qute-keepassxc"))

    ;; Nyxt browser Configuration fils
    ("nyxt/config.lisp"
     ,(home-file "files/nyxt" "config.lisp"))

    ("nyxt/keepassxc-pwi.lisp"
     ,(home-file "files/nyxt" "keepassxc-pwi.lisp"))

    ("nyxt/keepassxc-3431.lisp"
     ,(home-file "files/nyxt" "keepassxc-3431.lisp"))

    ;; A not so good way to lug around extensions too...
    ("nyxt/extensions/nx-invader-2/nx-invader-2.lisp"
     ,(home-file "files/nyxt/extensions/nx-invader-2" "nx-invader-2.lisp"))

    ("nyxt/extensions/nx-invader-2/nx-invader-2.asd"
     ,(home-file "files/nyxt/extensions/nx-invader-2" "nx-invader-2.asd"))

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


;; TODO - use define-configuration to create a better suited configuration interface using
;; the functionality of iambumblehead's home-alist.scm to provide a clean interface for users
;; to define xdg-local files to be symlinked into the XDG_HOME_DIR.
