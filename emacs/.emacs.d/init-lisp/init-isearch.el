(use-package isearch
  :straight nil
  :demand t
  :custom
  (isearch-lazy-count t)
  (isearch-lax-whitespace t)
  (search-whitespace-regexp (rx (minimal-match (0+ any))))
  :bind
  (:map isearch-mode-map
        ("C-l" . consult-line)))

(use-package isearch-mb
  :disabled t
  :straight t
  :demand t
  :bind
  (:map isearch-mb-minibuffer-map
        ("C-o" . loccur-isearch)
        ("C-l" . consult-line))
  :config
  (add-to-list 'isearch-mb--with-buffer #'loccur-isearch)
  (add-to-list 'isearch-mb--with-buffer #'consult-isearch-history)
  (add-to-list 'isearch-mb--after-exit  #'consult-line)
  (isearch-mb-mode))

(use-package loccur
  :disabled t
  :straight t
  :defer t)
