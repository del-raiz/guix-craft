;; raz-denote.el --- Enhanced Notetaking -*- lexical-binding: t; -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; Denote Configuration
;; https://protesilaos.com/emacs/denote


;; Code:






(use-package denote
  :hook (;;(find-file . denote-link-buttonize-buffer)
         (dired-mode . denote-dired-mode-in-directories)
         (denote-dired-mode . dired-hide-details-mode)
         (org-capture-mode . raz/denote-org-capture))

  :bind (("C-c n j" . raz/denote-journal)
         ("C-c n n" . denote))
  :init
  (setq raz/denote--dir "~/Documents/denotes/")
  :custom
  (denote-directory (expand-file-name raz/denote--dir))
  (denote-dired-directories (list
                             denote-directory
                             (expand-file-name "inbox" denote-directory)
                             (expand-file-name "research" denote-directory)
                             (expand-file-name "reference" denote-directory)
                             (expand-file-name "trash" denote-directory)))
  (denote-known-keywords '("emacs"
                           "ideas"
                           "journal"
                           "philosophy"
                           "projects"
                           "research"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type 'org)
  (denote-prompts '(title keywords subdirectory))
  (denote-date-prompt-use-org-read-date t)
  (denote-allow-multi-word-keywords t)
  (denote-date-format nil)
  (denote-link-fontify-backlinks t)
  :config
  (defun raz/denote-org-capture ()
    "Hook that configures denote for Org Capture."
    (:hook 'org-capture-mode-hook)

    (setq denote-org-capture-specifiers "%l\n%i\n%?)")

    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  (defun raz/denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal"))))



(provide 'raz-denote)
