(use-package consult
  :straight t
  :defer t
  :bind
  (:map isearch-mode-map
   ("C-l" . consult-line)))

(use-package vertico
  :straight t
  :defer t)

(use-package embark
  :straight t
  :defer t)

(use-package orderless
  :straight t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :defer t)
