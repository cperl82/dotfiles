(use-package windsize
  :straight t
  :defer t
  :commands (cp/hydra-windsize/body)
  :config
  (defhydra cp/hydra-windsize (nil nil)
    "resize"
    ("h" windsize-left  "left")
    ("j" windsize-down  "down")
    ("k" windsize-up    "up")
    ("l" windsize-right "right")))
