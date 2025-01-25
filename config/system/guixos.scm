(define-module (config system guixos)
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

(use-package-modules ssh cups suckless fonts wm xorg lisp lisp-xyz
                     guile guile-xyz file-systems linux audio wget
                     curl version-control compression)

(use-service-modules cups ssh desktop xorg guix networking)


;;; operating-system parameters

(define guixos-user-name "logoraz")

(define guixos-keyboard-layout
  (keyboard-layout "us"))

(define guixos-bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout guixos-keyboard-layout)))

(define guixos-swap-devices
  (list (swap-space
         (target
          (uuid "16d2cf76-fc27-4140-a980-feff050e1018")))))

(define guixos-file-systems
  (cons* (file-system
          (mount-point  "/boot/efi")
          (device (uuid "F8E9-9C22" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device (uuid "63be73df-47a0-4350-a592-e0541bece0bf" 'ext4))
          (type "ext4"))
	 %base-file-systems))

(define guixos-groups
  ;; Add the 'seat' group
  (cons
   (user-group (system? #t) (name "seat"))
   %base-groups))

(define guixos-users
  (cons* (user-account
          (name "logoraz")
          (comment "Crafter Bee")
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
;; Transformations not being used due to bug, stumpwm dependencies require
;; 2.4.7 fasl's (current in Guix)
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

(define guixos-base-packages
  (cons* sbcl ;(latest-sbcl sbcl)
	 ccl
         stumpwm+slynk
	 guile-colorized
         ;;intel-media-driver/nonfree
         bluez
         bluez-alsa
         brightnessctl
         lm-sensors
         openssh-sans-x
         git
         (list git "send-email")
         curl
         wget
         zip
         unzip
         %base-packages))

;;; System Services
;; Use Package substitutes instead of compiling everything & specify channels
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

(define guixos-base-services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout guixos-keyboard-layout)))

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
   
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;;; Define Operating system
(define guixos
  (operating-system
   ;; (inherit system)
   (host-name "locutus")
   (timezone "America/Los_Angeles")
   (locale "en_US.utf8")
   (keyboard-layout guixos-keyboard-layout)
   
   (kernel linux)
   (firmware (list linux-firmware))

   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   ;; for Lenovo ThinkPad X1 Carbon 4th Gen (Type 20FB) Laptop.
   (initrd microcode-initrd)
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))

   (bootloader guixos-bootloader)

   (swap-devices guixos-swap-devices)

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems guixos-file-systems)

   (groups guixos-groups)
   
   ;; List of user accounts ('root' is implicit).
   (users guixos-users)

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages guixos-base-packages)

   (services guixos-base-services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

;;; Instantiate Guix-OS
guixos
