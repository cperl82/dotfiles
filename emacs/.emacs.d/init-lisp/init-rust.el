(use-package rust-mode
  :straight t
  :defer t)

(use-package rustic
  :straight t
  :defer t
  :after rust-mode
  :hook
  ((rustic-mode . cp/enable-hideshow-and-hide-all)))
