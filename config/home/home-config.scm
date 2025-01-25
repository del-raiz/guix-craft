(define-module (config home home-config)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (config home services home-impure-symlinks))

(use-package-modules fonts web-browsers gnuzilla password-utils gnupg mail
                     gstreamer video compton image-viewers linux music
                     gnucash gimp inkscape graphics compression version-control
                     guile guile-xyz emacs emacs-xyz sdl compression
                     ;; added from system
                     lisp lisp-xyz wm xorg xdisorg freedesktop
                     ssh cups suckless networking package-management)

;;; Package Transformations
(define latest-nyxt
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

;;; Packages
(define guile-packages
  (list guile-next ;;|--> gnu packages guile
        guile-ares-rs ;;|--> gnu packages guile-xyz
        guile-hoot
        guile-websocket
        guile-sdl2 ;;|--> gnu package sdl
        sdl2))

(define logoraz-packages
  (list font-hack ;;|--> gnu packages fonts
        font-jetbrains-mono
        font-fira-code
        font-iosevka-aile
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        (latest-nyxt nyxt) ;;|--> gnu packages web-browsers :www-mail
        icecat             ;;|--> gnu packages gnuzilla
        keepassxc          ;;|--> gnu packages password-utils
        gnupg              ;;|--> gnu packages gnupg
        isync              ;;|--> gnu packages mail
        msmtp
        mu
        gstreamer ;;|--> gnu packages gstreamer
        gst-plugins-good
        gst-plugins-bad
        gst-libav
        mpv ;;|--> gnu packages video :apps
        vlc
        picom    ;;|--> gnu packages compton
        feh      ;;|--> gnu packages image-viewers
        pipewire ;;|--> gnu packages linux
        wireplumber
        lm-sensors
        brightnessctl
        playerctl ;;|--> gnu packages music
        gnucash   ;;|--> gnu packages gnucash
        gimp      ;;|--> gnu packages gimp
        inkscape  ;;|--> gnu packages inkscape
        blender   ;;|--> gnu packages graphics
        zip       ;;|--> gnu packages compression
        unzip
        git))     ;;|--> gnu packages version-control

(define emacs-packages
  (list  emacs                    ;;|--> gnu packages emacs
         emacs-diminish           ;;|--> gnu packages emacs-xyz
         emacs-delight
         emacs-nord-theme
         emacs-doom-themes
         emacs-nerd-icons
         emacs-doom-modeline
         emacs-ligature
         emacs-no-littering
         emacs-ws-butler
         emacs-undo-tree
         emacs-paredit
         emacs-visual-fill-column
         emacs-ace-window
         emacs-mct
         emacs-orderless
         emacs-corfu
         emacs-marginalia
         emacs-beframe
         emacs-denote
         emacs-magit
         emacs-vterm
         emacs-guix
         emacs-arei
         emacs-sly
         emacs-mbsync
         emacs-org-superstar
         emacs-org-appear
         emacs-erc-hl-nicks
         emacs-erc-image
         emacs-emojify))

(define stumpwm-packages
  (list sbcl-parse-float          ;;|--> gnu packages lisp-xyz
        sbcl-local-time
        sbcl-cl-ppcre
        sbcl-zpng
        sbcl-salza2
        sbcl-clx
        sbcl-zpb-ttf
        sbcl-cl-vectors
        sbcl-cl-store
        sbcl-trivial-features
        sbcl-global-vars
        sbcl-trivial-garbage
        sbcl-bordeaux-threads
        sbcl-cl-fad
        sbcl-clx-truetype
        sbcl-stumpwm-ttf-fonts     ;;|--> gnu packages wm; :stumpwm-contrib/util
        sbcl-stumpwm-kbd-layouts
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-globalwindows
        sbcl-stumpwm-cpu           ;;:stumpwm-contrib/modeline
        sbcl-stumpwm-mem
        sbcl-stumpwm-wifi
        sbcl-stumpwm-battery-portable))

(define x11-util-packages
  (list font-hack ;;|--> gnu packages fonts
        font-jetbrains-mono
        xterm ;;|--> gnu packages xorg
        transset
        xhost
        xset
        xsetroot
        xinput
        xrdb
        xrandr
        xclip ;;|--> gnu packages xdisorg
        xsel
        xss-lock
        xdg-utils ;;|--> gnu packages freedesktop
        blueman   ;;|--> gnu package networking
        bluez))

(define *home-path* "/home/logoraz/dotfiles/")


(define logoraz-home
  (home-environment
   ;; Below is the list of packages that will show up in your
   ;; Home profile, under ~/.guix-home/profile.
   (packages (append
              x11-util-packages
              stumpwm-packages
              guile-packages
              logoraz-packages
              emacs-packages))

   ;; Below is the list of Home services.  To search for available
   ;; services, run 'guix home search KEYWORD' in a terminal.
   (services
    (list
     (service home-pipewire-service-type)
     (service home-dbus-service-type) ;; for bluetooth --> system
     (simple-service 'home-impure-symlinks-dotfiles
                     home-impure-symlinks-service-type
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
                       ;; Nyxt Configuration Scaffolding
                       (".config/nyxt"
                        ,(string-append
                          *home-path*
                          "files/nyxt"))
                       (".local/share/nyxt/extensions"
                        ,(string-append
                          *home-path*
                          "files/nyxt/extensions"))))
     (simple-service 'env-vars home-environment-variables-service-type
                     '(("EDITOR" . "emacs")
                       ("BROWSER" . "nyxt")
                       ;; ("OPENER" . "opener.sh")
                       ("XDG_SESSION_TYPE" . "x11")
                       ("XDG_SESSION_DESKOP" . "stumpwm")
                       ("XDG_CURRENT_DESKTOP" . "stumpwm")
                       ;; ("XDG_DOWNLOAD_DIR" . "/home/logoraz/Downloads")
                       ("GUILE_WARN_DEPRECATED" . "detailed")))
     (service home-bash-service-type
              (home-bash-configuration
               (guix-defaults? #f)
               (aliases '(("grep" . "grep --color=auto")
                          ("ls"   . "ls -p --color=auto")
                          ("ll"   . "ls -l")
                          ("la"   . "ls -la")))
               (bashrc
                (list (local-file "dot-bashrc.sh"
                                  #:recursive? #t)))
               (bash-profile
                (list (local-file "dot-bash_profile.sh"
                                  #:recursive? #t)))))))))

;; Enable Home
logoraz-home
