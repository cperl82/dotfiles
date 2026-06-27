(use-package elisp-mode
  :defer t
  :straight nil
  :custom
  (lisp-loop-forms-indentation 3)
  (lisp-indent-function #'lisp-indent-function)
  :preface
  :hook
  ((emacs-lisp-mode . company-mode)
   (emacs-lisp-mode . cp/enable-hideshow-and-hide-all)))

(provide 'init-lisp)
