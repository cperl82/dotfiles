(use-package smartparens
  :straight t
  :demand t
  :preface
  (defun cp/smartparens-mode-setup ()
    (smartparens-mode)
    (smartparens-strict-mode))
  :config
  (sp-local-pair
   '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "'" nil :actions nil)
  (sp-local-pair
   '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "`" nil :actions nil)
  :hook
  ((lisp-data-mode . cp/smartparens-mode-setup)))
