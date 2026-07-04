(use-package elisp-mode
  :defer t
  :straight nil
  :custom
  (lisp-loop-forms-indentation 3)
  (lisp-indent-function #'lisp-indent-function)
  :preface
  (defun cp/emacs-lisp-mode-setup ()
    (company-mode)
    ;; flycheck-mode is nagging me about too many things currently
    ;(flycheck-mode)
    (cp/enable-hideshow-and-hide-all))
  (defalias 'cp/lisp-mode-setup 'cp/emacs-lisp-mode-setup)
  :hook
  ((emacs-lisp-mode . cp/emacs-lisp-mode-setup)
   (lisp-mode . cp/lisp-mode-setup)))
