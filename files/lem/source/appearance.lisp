(defpackage config/appearance
  (:use :cl :lem)
  (:export :set-opacity))

(in-package :config/appearance)


;;; SDL2 specific
#+lem-sdl2
(progn
  (defvar *opaque* nil
    "Transparency toggler for SDL2 frontend.")

  (defun set-opacity (opacity)
    "Set SDL2 opacity, aka transparency."
    (sdl2-ffi.functions:sdl-set-window-opacity
     (lem-sdl2/display:display-window (lem-sdl2/display:current-display)) 
     (coerce opacity 'single-float)))

  (define-command toggle-opacity () ()
    ;; FIXME: Weird bug (temp fix -- coerce)
    ;; The value 0.8 is not of type SINGLE-FLOAT when binding SB-ALIEN::VALUE
    (set-opacity (if *opaque* 1 0.8))
    (setf *opaque* (not *opaque*)))

  ;; Always start off as transparent
  (set-opacity 0.8))

