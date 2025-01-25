;; raz-lisp-ide.el --- Lisp IDE via SLY -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary:
;;

;; References
;; 1. http://joaotavora.github.io/sly/#A-SLY-tour-for-SLIME-users
;; 2. Source: https://github.com/joaotavora/sly
;;
;; TODO:
;;   1. Setup/Configure Nyxt extension
;;   2. Setup/Configure stumpwm-mode extension
;;   3. Look into:  M-. -> runs the command sly-edit-definition (found in sly-mode-map),
;;      which is an interactive native-compiled Lisp function in ‘sly.el’.


;; Code:

(use-package sly
  ;; Enable sly IDE for Common Lisp
  :hook ((lisp-mode . sly-editing-mode))
  ;; :custom
  ;; (inferior-lisp-program (executable-find "sbcl")
  ;;                        "Set default lisp to Steel Bank Common Lisp.")
  :config
  ;; Invoke SLY with a negative prefix argument, M-- M-x sly,
  ;; and you can select a program from that list.
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl")))
          (ccl (,(executable-find "ccl")))
          (clasp (,(executable-find "clasp")))))

  (defun raz/stumpwm-sly-connect ()
    "Auto connect to StumpWM slynk session -> port 4005."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    (save-excursion (sly-connect "localhost" 4005)))

  (defun raz/nyxt-sly-connect ()
    "Auto connect to Nyxt slynk session, start via start-slynk Nyxt command -> port 4006."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    (save-excursion (sly-connect "localhost" 4006)))

  (defun raz/chemscribe-sly-connect ()
    "Auto connect to Nyxt slynk session, start via start-slynk Nyxt command -> port 4006."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    (save-excursion (sly-connect "localhost" 4008)))

  ;; See: https://joaotavora.github.io/sly/#Loading-Slynk-faster
  (defun raz/sly-auto-connect ()
    (interactive)
    (unless (sly-connected-p)
      (save-excursion (sly)))))





(provide 'raz-lisp-ide)
