(define-module (config home guixos-home)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (guix gexp)
  #:use-module (config home services stumpwm-desktop)
  #:use-module (config home services environment)
  #:use-module (config home services xdg-files)
  #:use-module (config home services local-files)
  #:use-module (config home services emacs)
  #:use-module (config home services streaming)
  #:use-module (config home services udiskie)

  #:export (guixos-home))


(define guixos-home
  (home-environment
   ;; Below is the list of Home services.  To search for available
   ;; services, run 'guix home search KEYWORD' in a terminal.
   (services
    (append (list
	     ;; Enable bluetooth connections to be handled properly
	     ;; bluetooth service only currently available at system level.
	     (service home-dbus-service-type)

	     ;; Enable pipewire audio
	     (service home-pipewire-service-type)

	     ;; Monitor battery levels
	     (service home-batsignal-service-type)

	     ;; Udiskie for auto-mounting
	     (service home-udiskie-service-type)

	     ;; Streaming profile service
	     (service home-streaming-service-type)
	     
	     ;; Set environment variables for every session
	     (service home-env-vars-configuration-service-type)

	     ;; StumpWM Desktop profile configuration
	     (service home-stumpwm-desktop-service-type)

	     ;; Emacs Package profile configuration
	     (service home-emacs-config-service-type)

             ;; XDG files configuration
             (service home-xdg-local-files-service-type)
	     
	     ;; Enable symlinks of local files
             (service home-local-files-service-type)
	     
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
					  #:recursive? #t))))))
            %base-home-services))))

;; Enable Home
guixos-home
