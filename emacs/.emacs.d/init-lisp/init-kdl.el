(use-package kdl-mode
  :straight t
  :defer t
  :preface
  (defun cp/kdl-mode-setup ()
    (setq tab-width 4))
  :hook
  (kdl-mode . cp/kdl-mode-setup))
