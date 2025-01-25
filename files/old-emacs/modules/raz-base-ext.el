;;;; raz-base-ext.el --- Base Config/Defaults -*- lexical-binding: t -*-

;;; Author: Erik P. Almaraz

;;; Commentary/References:
;;; Core features of my configuration provided from external
;;; packages, i.e. obtained via Guix or Melpa

;;; Code:

;;; Configure package PATH's
(use-package no-littering)

(use-package ligature
  ;; Fonts & Theme Configuration
  ;; Fira Code & Ligature Support
  ;; See: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligature
  ;; See: https://github.com/mickeynp/ligature.el
  :diminish ligature-mode
  :config
  (dolist
      (face
       '((default :font "Fira Code" :height 110)
         (fixed-pitch :font "Fira Code" :height 110)
         (variable-pitch :font "Iosevka Aile" :height 110)))
    (raz/set-face-attribute (car face) (cdr face)))
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://" ";;;" ";;;;" "!!!" "!!!!"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Theme Configuration
;; Load in local copy of nord theme - to develop and customize...
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs/themes/"))
;; (load-theme 'kanagawa t)
;; https://github.com/tinted-theming/base16-emacs

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-spacegrey t)
  (load-theme 'doom-tomorrow-night t)


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package tab-bar
  :disabled
  :after nord-theme
  :config
    ;; Set custome definte "Nord" faces for tab-bar
  (dolist
      (face
       '((tab-bar :foreground "#7b88a1" :background "#272C37")
         (tab-line :inherit tab-bar)
         (tab-bar-tab :inherit mode-line-highlight
                      :foreground "#b48ead"
                      :background "#272C37")
         (tab-bar-tab :box (:line-width 1 :color "#7b88a1" :style none))
         (tab-bar-tab-group-current :inherit tab-bar-tab)
         (tab-bar-tab-group-current :box (:line-width 1 :color "#3B4252" :style none))
         (tab-bar-tab-inactive :foreground "#7b88a1" :background "#272C37")
         (tab-bar-tab-inactive :box (:line-width 1 :color "#616e88" :style none))
         (tab-bar-tab-group-inactive :inherit tab-bar-tab-inactive)
         (tab-bar-tab-ungrouped :inherit tab-bar-tab-inactive)))
    (raz/set-face-attribute (car face) (cdr face))))


;;; Editing/IDE Package configurations
(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-hist/"
                               *raz-var-directory*))))
  :config
  (setq kill-do-not-save-duplicates t)
  (global-undo-tree-mode))

(use-package paredit
  :diminish paredit-mode
  :hook ((eval-expression-minibuffer-setup
          lisp-interaction-mode
          emacs-lisp-mode
          lisp-mode
          scheme-mode
          org-mode) . enable-paredit-mode))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package magit
  :defer 5
  :custom
  (magit-clone-always-transient nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (vc-follow-symlinks t))

;;; Workflow frame/tab workspaces
(use-package beframe
  :diminish beframe-mode
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :custom
  (beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
  :config
  (beframe-mode 1))

(use-package ace-window
  :bind ("M-o" . 'ace-window))




(provide 'raz-base-ext)
