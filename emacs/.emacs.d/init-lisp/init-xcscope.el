(use-package xcscope
  :straight t
  :defer t
  :custom
  (cscope-option-use-inverted-index t)
  (cscope-edit-single-match nil)
  (cscope-option-kernel-mode t))

(provide 'init-xcscope)
