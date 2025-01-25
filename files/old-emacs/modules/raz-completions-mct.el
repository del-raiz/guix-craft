;; raz-completions-mct.el --- Completions Framework -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;;   See built-in completion UI framework `icomplete', `ido', `fido'


;; Code:
;;TODO - define variables using `use-package' `:custom' keyword where possible.

(use-package orderless
  ;;https://github.com/oantolin/orderless
  :custom
  ;; Need to verify this doesn't conflict with mct mode
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :diminish marginalia-mode
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;;FIXME - Investigate further mode-line active/inactive bug and report or try to
;;        fix.
;;ISSUE - Mode Line in window stays active when focus has switched to
;;        completions buffer - seems to happen when completions buffer is
;;        automatically updated or manually requested through "TAB" activation...
;;

(use-package mct
  :diminish mct-mode
  :demand t
  :bind (:map minibuffer-local-filename-completion-map
              ("DEL" . raz/backward-updir)
              :map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?"   . nil))
  :custom
  (mct-completion-window-size (cons 'mct-frame-height-third 1))
  (mct-remove-shadowed-file-names t)
  (mct-hide-completion-mode-line t)
  (mct-live-completion t)
  (mct-minimum-input 3)
  (mct-live-update-delay 0.6)
  (mct-persist-dynamic-completion t)
  (mct-completion-passlist nil)
  (mct-completion-blocklist nil)
  (completion-styles '(basic substring initials flex partial-completion orderless))
  (completion-category-overrides '((file
                                    (styles . (basic
                                               partial-completion
                                               orderless)))))
  :config
  ;; Prot's adaptation of `icomplete-fido-backward-updir'.
  (defun raz/backward-updir ()
    "Delete char before point or go up a directory."
    (interactive nil mct-mode)
    (cond
     ((and (eq (char-before) ?/)
           (eq (mct--completion-category) 'file))
      (when (string-equal (minibuffer-contents) "~/")
        (delete-minibuffer-contents)
        (insert (expand-file-name "~/"))
        (goto-char (line-end-position)))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          (delete-region (1+ (point)) (point-max)))))
     (t (call-interactively 'backward-delete-char))))

  (mct-mode))


;; Completions
;;FIXME: Had to install via 'gnu' archive, something was going wrong with melpa - also,
;; package archives did not respect alist order - should have check gnu first,
;; is there a way to set a prefered archive/source for 'packages'.
(use-package corfu
  :diminish corfu-mode
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-cycle t "Enable corfu cycling...")
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  :config
  ;; Needed for customization/control of other variables that interfere with corfu
  (setq text-mode-ispell-word-completion nil
        read-extended-command-predicate #'command-completion-default-include-p
        tab-always-indent 'complete
        completion-cycle-threshold 3))




(provide 'raz-completions-mct)
