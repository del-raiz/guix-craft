(define-module (config system system-config)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix ci)
  #:use-module (guix transformations)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-system-modules keyboard nss)

(use-package-modules ssh cups suckless fonts wm lisp lisp-xyz
                     file-systems linux audio version-control
                     wget curl compression xorg xdisorg)

(use-service-modules cups ssh desktop xorg
                     guix networking)


;;; operating-system parameters

(define %guixos-user-name "logoraz")

(define %guixos-keyboard-layout
  (keyboard-layout "us"))

(define %guixos-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout %guixos-keyboard-layout)))

(define %guixos-swap-devices
  (list (swap-space
         (target
          (uuid "08b16346-aa6b-4744-9856-6070d78e38ca")))))

(define %guixos-file-systems
  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid "F8E9-9C22"
			'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "872f93f3-bcb4-4907-bb4c-d6002496d3a8"
			'ext4))
          (type "ext4"))
	 %base-file-systems))

(define %guixos-groups
  ;; Add the 'seat' group
  (cons
   (user-group (system? #t) (name "seat"))
   %base-groups))

(define %guixos-users
  (cons* (user-account
          (name "logoraz")
          (comment "Worker Bee")
          (home-directory "/home/logoraz")
          (group "users")
          (supplementary-groups '("wheel"    ;; sudo
                                  "netdev"   ;; network devices
                                  "tty"      ;; -
                                  "input"    ;; -
                                  "lp"       ;; control bluetooth devices
                                  "audio"    ;; control audio devices
                                  "video"))) ;; control video devices
         %base-user-accounts))


;;; Packages & Transformations

;; ref: https://guix.gnu.org/manual/en/guix.html#Defining-Package-Variants
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

(define %guixos-base-packages
  (cons* sbcl ;(latest-sbcl sbcl)
         stumpwm+slynk
         bcachefs-tools

         ;; tools
         pipewire
         wireplumber
         bluez
         brightnessctl
         lm-sensors
         openssh-sans-x
         git
         (list git "send-email")
         curl
         wget
         zip
         unzip

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

         %base-packages))


;;; System Services

;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define (substitutes->services config)
  (guix-configuration
   (inherit config)
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
           "https://ci.guix.gnu.org"
           %default-substitute-urls))
   (authorized-keys
    (cons* (origin
            (method url-fetch)
            (uri "https://substitutes.nonguix.org/signing-key.pub")
            (file-name "nonguix.pub")
            (sha256
             (base32
              "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
           %default-authorized-guix-keys))))

(define %guixos-base-services
  (cons*
   ;; Ref: https://guix.gnu.org/manual/en/html_node/X-Window.html
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout %guixos-keyboard-layout)))

   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #f)))

   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))

   ;; ssh user@host -p 2222
   (service openssh-service-type
            (openssh-configuration
             (openssh openssh)
             (port-number 2222)))

   ;; TODO: New - need to look into & configure!!
   (service tor-service-type)

   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;; Define GuixOS StumpWm - Crystallized Momentum

(define guixos-stumpwm
  (operating-system
   ;; (inherit %base-system)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout %guixos-keyboard-layout)

   (kernel linux)
   (firmware (list linux-firmware))

   (initrd microcode-initrd)
   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   ;; for Lenovo ThinkPad X1 Carbon 4th Gen (Type 20FB) Laptop.
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))

   (bootloader %guixos-bootloader)

   (swap-devices %guixos-swap-devices)

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems %guixos-file-systems)
   
   (groups %guixos-groups)

   ;; List of user accounts ('root' is implicit).
   (users %guixos-users)

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages %guixos-base-packages)

   (services %guixos-base-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))


;;; Instantiate GuixOS StumpWM Crystallized Momentum
guixos-stumpwm
