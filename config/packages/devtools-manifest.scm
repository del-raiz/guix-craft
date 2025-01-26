;;;; devtools-manifest.scm
;;; To be used with guix shell command as follows
;;;
;;; guix shell -m devtools-manifest.scm
;;;
;;; or can invoke with a specific program
;;;
;;; guix shell -m devtools-manifest.scm -- emacs
;;;

(use-modules (gnu packages))

(specifications->manifest
 '("gcc-toolchain"
   "binutils"
   "make"
   "curl"
   "git:send-email"))
