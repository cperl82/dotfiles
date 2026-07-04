(use-package smartparens
  :straight t
  :demand t
  :preface
  (defun cp/smartparens-mode-setup ()
    (smartparens-mode)
    (smartparens-strict-mode))
  :hook
  ((lisp-data-mode . cp/smartparens-mode-setup)))
