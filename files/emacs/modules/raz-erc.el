;; raz-erc.el --- IRC Framework -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References:

(setq erc-server "irc.libera.chat"
      erc-nick "logoraz"
      erc-user-full-name "Erik Almaraz"
      erc-track-shorten-start 8
      ;;    erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-fill-function 'erc-fill-static
      ;;      erc-track-exclude-server-buffer t
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
      erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
      erc-pals '("daviwil" "Fade" "SummerEmacs" "benoitj" "BigEatie")
      erc-fill-column 75)


(use-package erc-hl-nicks
  ;; :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  ;; :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(use-package emojify
  ;; :ensure t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(provide 'raz-erc)
