(define-module (config systems base-system)
  #:use-modules (gnu)
  #:use-modules (guix)
  #:use-module (nongnu packages linux)
  #:export (base-system))

(use-package-modules ssh)
(use-service-modules networking ssh)

(define base-system
  (operating-system
    (host-name "base-system")
    (timezone "America/Los_Angeles")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us"))

    (kernel linux)
    (kernel-arguments %default-kernel-arguments)
    (initrd (lambda (file-systems . rest)
              ;; Create a standard initrd but set up networking
              ;; with the parameters QEMU expects by default.
              (apply base-initrd file-systems
                     #:qemu-networking? #t
                     rest)))
    ;; Don't include any default firmware
    (firmware '())

    ;; The bootloader and file-systems fields here will be replaced by
    ;; the exact same values in the gemini and taurus configurations,
    ;; but in practice these fields will depend on each machine's
    ;; partition configuration.
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/vda"))
                 (terminal-outputs '(console))))

    (file-systems (cons (file-system
                          (mount-point "/")
                          (device "/dev/vda1")
                          (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "crafter")
                  (comment "Worker Bee")
                  (password (crypt "crafter" "$6$abc"))
                  (group "users")
                  (supplementary-groups '("wheel" "netdev"
                                          "audio" "video")))
                 %base-user-accounts))

    (services (cons* (service dhcp-client-service-type)
                     (service openssh-service-type)
                     %base-services))))
