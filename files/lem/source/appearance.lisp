(defpackage config/appearance
  (:use :cl :lem)
  (:export :set-opacity))

(in-package :config/appearance)

(defconstant +regular-font+ #p"/home/logoraz/.local/share/fonts/FiraCodeNerdFontMono-Regular.ttf")

(defconstant +bold-font+ #p"/home/logoraz/.local/share/fonts/FiraCodeNerdFontMono-Bold.ttf")

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

;; Use FiraCode Nerd fonts
#+lem-sdl2
(ignore-errors
  (let ((font-regular +regular-font+)
        (font-bold +bold-font+))
    (if (and (uiop:file-exists-p font-regular)
             (uiop:file-exists-p font-bold))
        (lem-sdl2/display:change-font (lem-sdl2/display:current-display)
                                      (lem-sdl2/font:make-font-config
                                       :latin-normal-file font-regular
                                       :latin-bold-file font-bold
                                       :cjk-normal-file font-regular
                                       :cjk-bold-file font-bold))
        (message "Fonts not found."))))

