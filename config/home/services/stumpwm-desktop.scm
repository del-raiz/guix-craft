(define-module (config home services stumpwm-desktop)
  #:use-module (gnu)                             ; -
  #:use-module (gnu packages lisp)               ; -
  #:use-module (gnu packages lisp-xyz)           ; - StumpWM Contrib
  #:use-module (gnu packages fonts)              ;-> font packages
  #:use-module (gnu packages wm)                 ;->
  #:use-module (gnu packages networking)         ;-> blueman
  #:use-module (gnu packages xorg)               ; -
  #:use-module (gnu packages xdisorg)            ; -
  #:use-module (gnu packages compton)            ;-> picom
  #:use-module (gnu packages image-viewers)      ;-> feh
  #:use-module (gnu packages freedesktop)        ;-> udiskie,
  #:use-module (gnu packages linux)              ; -
  #:use-module (gnu packages glib)               ; -
  #:use-module (gnu packages gnome)              ; -
  #:use-module (gnu packages gnome-xyz)          ; -
  #:use-module (gnu packages kde-frameworks)     ; -
  #:use-module (gnu packages text-editors)       ;-> lem
  #:use-module (gnu packages sdl)                ;-> sdl2 (for lem)
  #:use-module (gnu packages web-browsers)       ;-> nyxt
  #:use-module (gnu packages gnuzilla)           ;-> icecat
  #:use-module (gnu packages gstreamer)          ; -
  #:use-module (gnu packages compression)        ; -
  #:use-module (gnu packages gnuzilla)           ; -
  #:use-module (gnu packages terminals)          ; -
  #:use-module (gnu packages graphics)           ; -
  #:use-module (gnu packages image)              ; -
  #:use-module (gnu packages music)              ; -
  #:use-module (gnu packages video)              ; -
  #:use-module (gnu packages package-management) ; -
  #:use-module (gnu packages password-utils)     ;-> password-store
  #:use-module (gnu packages gnupg)              ;-> gnupg
  #:use-module (gnu packages gnucash)            ;-> gnucash
  #:use-module (gnu packages gimp)               ;-> gimp
  #:use-module (gnu packages inkscape)           ;-> inkscape
  #:use-module (gnu packages pdf)                ;-> zathura
  #:use-module (gnu packages shellutils)         ;-> trash-cli
  #:use-module (gnu services configuration)      ; -
  #:use-module (gnu home services)               ; -
  #:use-module (guix gexp)                       ; -
  #:use-module (guix transformations)            ;-> options-transformations

  #:export (home-stumpwm-desktop-service-type))


;;; Package Transformations
;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants
(define curr-trash-cli
  ;; Currently failing build due to tests since update to latest python...
  (options->transformation
   '((without-tests . "trash-cli"))))

(define latest-nyxt
  ;; use rde/packages/web-browsers.scm
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

(define (home-stumpwm-desktop-profile-service config)
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
        sbcl-stumpwm-battery-portable

	;; X11/Xorg
	xterm ;;|--> gnu packages xorg
        transset
        xhost
        xset
        xsetroot
        xinput
        xrdb
        xrandr
        xclip     ;;|--> gnu packages xdisorg
        xsel
        xss-lock
        xdg-utils ;;|--> gnu packages freedesktop
	picom     ;;|--> gnu packages compton
        feh       ;;|--> gnu packages image-viewers

        ;; XDG Utilities
        flatpak
        xdg-desktop-portal
        xdg-desktop-portal-gtk
        xdg-utils ;; For xdg-open, etc
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        adwaita-icon-theme
        breeze-icons ;; for KDE apps
        gnome-themes-extra
        ;; bibata-cursor-theme

        ;; Fonts
        font-jetbrains-mono
        font-fira-code
        font-hack
        font-liberation
        font-iosevka-aile
        font-awesome
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk

        ;; Browsers
        (latest-nyxt nyxt)
	icecat

        ;; Editors/IDE's
        lem
        sdl2
        
        ;; Authentication
        gnupg
        pinentry
        keepassxc
        password-store ;; move to password-store eventually...

        ;; Audio devices & Media playback
        mpv ;;|--> gnu packages video
        mpv-mpris
	vlc
        youtube-dl
        playerctl
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        pipewire ;;|--> gnu packages linux
        wireplumber

        ;; PDF reader
        zathura
        zathura-pdf-mupdf

        ;; Applications
        gnucash   ;;|--> gnu packages gnucash
        gimp-next ;;|--> gnu packages gimp
        inkscape  ;;|--> gnu packages inkscape
        blender   ;;|--> gnu packages graphics

        ;; Utilities
        (curr-trash-cli trash-cli)
        blueman
        network-manager-applet
        udiskie))

(define home-stumpwm-desktop-service-type
  (service-type (name 'home-sway-desktop-config)
                (description "Applies my personal Sway desktop configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-stumpwm-desktop-profile-service)))
                (default-value #f)))
