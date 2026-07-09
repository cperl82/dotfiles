(use-package isearch
  :straight nil
  :demand t
  :custom
  (isearch-lazy-count t)
  (isearch-lax-whitespace t)
  (search-whitespace-regexp (rx (minimal-match (0+ any))))
  :bind
  (:map isearch-mode-map
   ("C-o" . isearch-occur)
   ("C-d" . isearch-forward-thing-at-point)))
