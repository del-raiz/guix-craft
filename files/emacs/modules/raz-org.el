;; raz-org.el --- Advanced Office Tools -*- lexical-binding: t; -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; TODO - Enable Spell Check for Org Mode
;;      - Use either Ispell, Aspell, or Enchant (Research)...

;; References:
;; 1. Babel Languages: https://orgmode.org/worg/org-contrib/babel/languages/index.html
;; 2. tbd
;; 3. tbd
;; 4. tbd



;; Code:

(use-package org
  ;; :diminish org-mode
  :hook ((org-mode . raz/org-fonts-hkf)
         ;(org-mode . raz/org-latex-hkf)
         )
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-link-descriptive t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quite t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-sticky t)
  (org-agenda-sticky t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-use-fast-todo-selection t)
  (org-tag-alist '((:startgroup)         ; Set custom tags
                  ;; Put mutually exclusive tags here
                  (:endgroup)
                  ("@home"      . ?H)
                  ("@office"    . ?O)
                  ("@errands"   . ?E)
                  ("@traveling" . ?T)
                  ("@phone"     . ?P)
                  ("@email"     . ?M)))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)")
     (sequence "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)")
     (sequence "ACTIVE")))
  (org-todo-keyword-faces
   '(("TODO" . "#EBCB8B")          ; Yellow
     ("NEXT" . "#D08770")          ; Orange
     ("WAIT" . "#B48EAD")          ; Purple
     ("HOLD" . "#BF616A")          ; Red
     ("DONE" . "#A3BE8C")          ; Green
     ("ACTIVE"    . "#88C0D0")     ; Teal
     ("INACTIVE"  . "#81A1C1")     ; Light Blue
     ("CANCELLED" . "#5E81AC")))   ; ?
  (org-babel-lisp-eval-fn 'sly-eval "Configure Babel Programming Language Execution")
  :config
  ;; Org Helper Hook Functions
  (defun raz/org-fonts-hkf ()
    "Hook function enabling Org faces/fonts."
    ;; Set faces for heading levels
    (dolist
        (face
         '((org-document-title extra-bold 1.40)
           (org-level-1 regular 1.30)
           (org-level-2 regular 1.15)
           (org-level-3 regular 1.08)
           (org-level-4 regular 1.04)
           (org-level-5 regular 1.02)
           (org-level-6 regular 1.01)
           (org-level-7 regular 1.00)
           (org-level-8 regular 1.00)))
      (set-face-attribute (car face) nil
                          :inherit 'variable-pitch
                          :weight (cadr face)
                          :height (caddr face)))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (dolist
        (face
         '((org-table    fixed-pitch)
           (org-formula  fixed-pitch)
           (org-checkbox fixed-pitch)
           (org-table    (shadow fixed-pitch))
           (org-verbatim (shadow fixed-pitch))
           (org-special-keyword (shadow fixed-pitch))
           (org-meta-line (font-lock-comment-face fixed-pitch))
           (line-number  fixed-pitch)
           (line-number-current-line fixed-pitch)))
      (set-face-attribute (car face) nil
                          :inherit (cadr face))))

  (defun raz/org-latex-hkf ()
    "Hook function setting up configuration for Org using Latex."

    (setq org-latex-listings t
          org-latex-pdf-process '("latexmk -pdf -outdir=%o %f")
          org-export-with-smart-quotes t)

    (with-eval-after-load 'ox-latex
      (add-to-list 'org-latex-classes
                   '("org-plain-latex"
                     "\\documentclass{article}
                        [NO-DEFAULT-PACKAGES]
                        [PACKAGES]
                        [EXTRA]"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

  ;; Org Babel Settings
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (scheme . t)))

  (dolist (lang
           '(("conf-unix" . conf-unix)
             ("conf-xorg" . conf-xdefaults)
             ("lisp"      . lisp)))
    (push lang org-src-lang-modes)))

;; Org Accessories
(use-package org-indent
  ;; :diminish org-indent-mode
  :after org)

(use-package org-tempo
  ;; :diminish org-tempo-mode
  :after org
  :config
  (setq org-structure-template-alist
        '(("el"  . "src emacs-lisp")
          ("li"  . "src lisp")
          ("sc"  . "src scheme")
          ("sh"  . "src sh")
          ("co"  . "src conf")
          ("C"   . "src C")
          ("bib" . "src bibtex")
          ("cm"  . "comment"))))

;;; External Packages
;; TODO: org-appear does not seem to be working... FIX!!
(use-package org-appear
  ;; :diminish org-appear-mode
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-trigger 'always)
  (org-appear-delay 0.2)
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package org-superstar
  ;; :diminish org-superstar-mode
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  ;; :diminish visual-line-mode
  :hook (org-mode . visual-line-fill-column-mode))





(provide 'raz-org)
