;; raz-subrx.el --- Emacs Lisp Subroutines Xtra -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; + https://spritely.institute/static/papers/scheme-primer.html
;;


;;
;; Helper Functions
;;

;; Facile passing of lists to `set-face-attribute', use only in theme setting.
(defun raz/set-face-attribute (face spec)
  "Set attributes FACE from SPEC.
FACE is expected to be a symbol with the same faces
as accepted by `set-face-attribute'.
SPEC is expected to be a plist with the same key names
as accepted by `set-face-attribute'.
FRAME is always set to nil"
  (when (and face spec)
    (apply 'set-face-attribute face nil spec)))


(defun list->add-to-list (list list-var)
  "Add list of items to list via`add-to-list'."
  (dolist (item list)
    (add-to-list list-var item)))

;;
;; Macros
;;

;; Generalize Customize Variable Definitions...
(defmacro raz/setq-customize (&rest args)
  "Set the default for variable VAR to VALUE employing 'customize-set-variable'.
See `customize-set-variable' for specifics on VAR and VALUE. Note VAR does not
need to be quoted as in 'customize-set-variable'

In general, one can use multiple variables and values, as in
  (del-emacs-setq-customize VAR VALUE [COMMENT] VAR VALUE [COMMENT]...)
This sets each VAR's default value to the corresponding VALUE.
The VALUE for the Nth VAR can refer to the new default values of previous VARs.

\(fn [VAR VALUE]...)"
  (declare (indent nil))
  (let (var val com exps)
    (while args
      (setq var (pop args)
            val (pop args))
      (if (not (stringp (car args)))
          (push `(customize-set-variable ',var ,val) exps)
        (setq com (pop args))
        (push `(customize-set-variable ',var ,val ,com) exps)))
    `(progn . ,(nreverse exps))))




;; Keep Hook Functions & Hooks Hygenic...
(defmacro raz/defhook (symbol &rest body)
  "Define SYMBOL as a function to be passed to hook(s) that are required to be
defined in BODY via the :hook keyword.
SYMBOL is the 'hook' function name, it should not be quoted.
BODY comprises first of keywords, including mandatory :hook, followed by
the forms associated with the 'hook' function body. Keywords should have
the form:

([KEYWORD VALUE])..., where VALUE must be a quoted symbol or list.

The following keywords are meaninful:

:hook  VALUE should be a variable type designating the hook which function named
       SYMBOL should be associated with. VALUE may be a single hook, or a list of
       hooks.
:depth VALUE should conform `add-hook' spec for optional values.
:local VALUE should conform `add-hook' spec for optional values.
:defer VALUE should be an integer type designating the time in seconds to wait
       after hook has been called before running body of function named SYMBOL.
:if    VALUE tbd...
:tbd   tbd...

\(fn SYMBOL [DOCSTRING] BODY...)"
  (declare (doc-string 2) (indent defun))
  (let (func doc (if-clause t) hooks (depth 0) local time exps)
    (dolist (element body)
      (when (stringp element)
        (setq doc element
              body (delq element body)))
      (when (and (plistp element) (keywordp (car element)))
        (pcase (car element)
          (:hook (push (cadr (plist-get element :hook)) hooks)
                 (setq body (delq element body)
                       hooks (flatten-list hooks)))
          (:depth (setq depth (plist-get element :depth)
                        body (delq element body)))
          (:local (setq local (plist-get element :local)
                        body (delq element body)))
          (:if (setq if-clause (plist-get element :if)
                     body (delq element body)))
          (:defer (setq time (plist-get element :defer)
                        body (delq element body))))))
    (when if-clause
      (if time (setq body `((run-at-time ,time nil (lambda nil ,@body)))))
      (if (and doc (>= (length doc) 1)) (push `(defun ,symbol nil ,doc ,@body) exps)
        (push `(defun ,symbol nil ,@body) exps))
      (while hooks
        (let (hook)
          (setq hook (pop hooks))
          (push `(add-hook ',hook ',symbol ,depth ,local) exps)))
      `(progn . ,(nreverse exps)))))

;; Samples of `del-emacs-defhook' (expand with `pp-macroexpand-last-sexp' or
;; with `emacs-lisp-macroexpand')
;;
;; Single Hook Case
;; (del-emacs-defhook my-hook-func
;;   "Hook function for testings"
;;   (:hook 'only-hook)
;;   (:depth 'append)
;;   (:local 'local)
;;   (:defer 10)
;;   (message "I am here!!!"))
;;
;; OR this works as well... maybe an unintended side-effect, perhaps going
;;    through the multi-case code base...
;; (del-emacs-defhook my-hook-func
;;   "Hook function for testings"
;;   (:hook '(only-hook))
;;   (:depth 'append)
;;   (:local 'local)
;;   (:defer 10)
;;   (message "I am here!!!"))
;;
;; Multiple Hook Case
;; (del-emacs-defhook my-hook-func
;;   "Hook function for testings"
;;   (:hook '(first-hook second-hook third-hook))
;;   (:depth 'append)
;;   (:local 'local)
;;   (:defer 10)
;;   (message "I am here!!!"))




(provide 'raz-subrx)
