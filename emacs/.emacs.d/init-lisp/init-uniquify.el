(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator " @")
  (uniquify-strip-common-suffix nil)
  (uniquify-after-kill-buffer-p t))
