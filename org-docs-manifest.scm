;;;; org-docss-manifest.scm
;;; To be used with guix shell command as follows
;;;
;;; guix shell -m org-docs-manifest.scm -- emacs
;;;

(specifications->manifest
 '("texlive-scheme-basic"
   "texlive-collection-latexrecommended"
   "texlive-collection-fontsrecommended"))