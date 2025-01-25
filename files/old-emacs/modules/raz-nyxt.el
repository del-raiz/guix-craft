;; raz-nyxt.el --- Emacs Default Nyxt Browser  -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; 1. Source
;; https://www.reddit.com/r/emacs/comments/limazh/set_nyxt_ast_default_external_browser_usepackage/

(use-package browse-url
  :commands browse-url-nyxt
  :init
  (defun browse-url-nyxt (url &optional new-window)
    "Ask the Nyxt WWW Browser to load URL. Default to the URL around or before point.
The strings in variable `browse-url-nyxt-arguments' are also passed to Nyxt.
When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Nyxt window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-nyxt-new-window-is-buffer' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
    (interactive (browse-url-interactive-arg "URL: "))
    (let* ((url (browse-url-encode-url url))
	   (process-environment (browse-url-process-environment))
	   (process
            (apply 'start-process
		   (concat "nyxt " url)
		   nil
		   browse-url-nyxt-program
		   ;;TODO make this work (how to launch make-window (or window-make;) usikg "-e"?)
		   ;;(if (and (not (or (browse-url-maybe-new-window new-window)
		   ;;		     (browse-url-nyxt-new-window-is-buffer)))
		   ;;	    (browse-url-new-window-flag)))
		   (append
		    browse-url-nyxt-arguments
		    (list url)))))
      (set-process-sentinel process
			    `(lambda (process change)
			       (browse-url-nyxt-sentinel process ,url)))))

  (function-put 'browse-url-nyxt 'browse-url-browser-kind 'external)

  (defun browse-url-nyxt-sentinel (process url)
    "Handle a change to the process communicating with Nyxt."
    (or (eq (process-exit-status process) 0)
        (let* ((process-environment (browse-url-process-environment)))
	  ;; Nyxt is not running - start it
	  (message "Starting %s..." browse-url-nyxt-program)
	  (apply 'start-process (concat "nyxt " url) nil
	         browse-url-nyxt-program
	         (append browse-url-nyxt-startup-arguments (list url))))))

  (defcustom browse-url-nyxt-program
    (let ((candidates '("nyxt")))
      (while (and candidates (not (executable-find (car candidates))))
	(setq candidates (cdr candidates)))
      (or (car candidates) "nyxt"))
    "The name by which to invoke the Nyxt browser."
    :type 'string
    :version "28.1")

  (defcustom browse-url-nyxt-arguments nil
    "A list of strings to pass to Nyxt as arguments."
    :type '(repeat (string :tag "Argument"))
    :version "28.1")
  :custom
  (browse-url-browser-function 'browse-url-nyxt))


(provide 'raz-nyxt)
