;;; Borrowed from:
;;; https://codeberg.org/daviwil/dotfiles/daviwil/home-services/streaming.scm
(define-module (config home services streaming)
  #:use-module (nongnu packages video) ; -
  #:use-module (config packages video)
  #:use-module (gnu home services)     ; -

  #:export (home-streaming-service-type))

(define (home-streaming-profile-service config)
  (list obs-with-cef
        obs-vaapi))

(define home-streaming-service-type
  (service-type (name 'home-streaming)
                (description "Packages and configuration for streaming with OBS.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-streaming-profile-service)))
                (default-value #f)))
