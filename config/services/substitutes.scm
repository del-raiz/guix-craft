(define-module (config services substitutes)
  #:use-module (ice-9 optargs)                   ;-> keyword args
  #:use-module (ice-9 ftw)                       ;-> wip
  #:use-module (gnu)                             ;-> guix-configuration
  #:use-module (gnu packages package-management) ;-> guix-for-channels
  #:use-module (guix gexp)                       ; -
  #:use-module (guix ci)                         ; -
  #:use-module (guix packages)                   ; -
  #:use-module (guix download)                   ; -

  #:export (substitutes->services
            %guixos-substitute-urls
            %guixos-authorized-keys))


(define %guixos-substitute-urls
  (cons* "https://substitutes.nonguix.org"
         "https://ci.guix.gnu.org"
         %default-substitute-urls))

(define %guixos-authorized-keys
  (cons* (origin
          (method url-fetch)
          (uri "https://substitutes.nonguix.org/signing-key.pub")
          (file-name "nonguix.pub")
          (hash
           (content-hash
            "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
         %default-authorized-guix-keys))

;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/\
;; Getting-Substitutes-from-Other-Servers.html
(define* (substitutes->services config #:key channels)
  (guix-configuration
   (inherit config)
   (channels channels)
   ;; ref https://guix.gnu.org/manual/devel/en/html_node/\
   ;;     Customizing-the-System_002dWide-Guix.html
   (guix (guix-for-channels channels))
   (substitute-urls %guixos-substitute-urls)
   (authorized-keys %guixos-authorized-keys)))
