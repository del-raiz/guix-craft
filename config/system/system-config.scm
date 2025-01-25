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

(use-system-modules keyboard)
(use-package-modules ssh cups suckless fonts wm lisp lisp-xyz)
(use-service-modules cups ssh desktop xorg)

(define locutus-keyboard-layout
  (keyboard-layout "us"))

;; Transformations not being used due to bug, stumpwm dependencies require
;; 2.4.7 fasl's (current in Guix)
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

(define stumpwm-base-packages
  (list sbcl ;(latest-sbcl sbcl)
        stumpwm+slynk))

;; System Services
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

(define guix-system-services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout locutus-keyboard-layout)))
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
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;;; Define Operating system
(define logoraz-user-account
  (list (user-account
         (name "logoraz")
         (comment "Erik P. Almaraz")
         (group "users")
         (home-directory "/home/logoraz")
         (supplementary-groups '("wheel" "netdev" "audio" "video" "lp")))))

(define locutus-file-system
  (list (file-system
         (mount-point  "/boot/efi")
         (device (uuid "F8E9-9C22" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid "63be73df-47a0-4350-a592-e0541bece0bf" 'ext4))
         (type "ext4"))))

(define guix-os
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))
   (firmware (list linux-firmware))
   (locale "en_US.utf8")
   (timezone "America/Los_Angeles")
   (keyboard-layout locutus-keyboard-layout)
   (host-name "locutus")

   ;; List of user accounts ('root' is implicit).
   (users (append
           logoraz-user-account
           %base-user-accounts))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (swap-devices (list (swap-space
                        (target
                         (uuid
			  "16d2cf76-fc27-4140-a980-feff050e1018")))))

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems (append
                  locutus-file-system
		  %base-file-systems))

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages (append stumpwm-base-packages
                     %base-packages))

   (services guix-system-services)))

;;; Instantiate Guix-OS
guix-os
