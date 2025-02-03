;;;; Theme settings for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;


(in-package :stumpwm)

;;; Fonts

;; Enable TTF fonts
(load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-system-path+ "fonts/")
                            (concat +guix-home-path+ "fonts/")
                            (concat +xdg-data-home-path+ "fonts/"))
      clx-truetype:+font-cache-filename+ (concat +xdg-data-home-path+
                                                 "fonts/"
                                                 "font-cache.sexp"))
(xft:cache-fonts)
(set-font `(,(make-instance ;; f0
              'xft:font :family "Hack"
                        :subfamily "Regular" :size 11 :antialias t)
            ,(make-instance ;; f1
              'xft:font :family "JetBrains Mono"
                        :subfamily "Regular" :size 11 :antialias t)
            ,(make-instance ;; f2
              'xft:font :family "Symbols Nerd Font Mono"
                        :subfamily "Regular" :size 13 :antialias t)
            ,(make-instance ;; f3
              'xft:font :family "FontAwesome"
              :subfamily "Regular" :size 13 :antialias t)))

;;; Colors
(setq *colors* (list
                (list logoraz-nord0 logoraz-nord1)    ; 0 Black
                logoraz-nord11                        ; 1 Red
                logoraz-nord14                        ; 2 Green
                logoraz-nord13                        ; 3 Yellow
                logoraz-nord10                        ; 4 Dark Blue
                logoraz-nord14                        ; 5 Magenta -> 'Green'
                logoraz-nord8                         ; 6 Cyan
                (list logoraz-nord5 logoraz-nord6)    ; 7 White
                ;; Extra Colors
                logoraz-nord12                        ; 8 optional-1 - 'Orange'
                logoraz-nord15))                      ; 9 optional-2 - 'Purple'

(when *initializing*
  (update-color-map (current-screen)))
