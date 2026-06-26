(use-package emacs
  :custom
  (ad-redefinition-action 'accept)
  (column-number-mode t)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (load-prefer-newer t)
  (make-backup-files nil)
  (use-short-answers t)
  :config
  (put 'narrow-to-region 'disabled nil)
  (global-auto-revert-mode)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :hook
  ((prog-mode . turn-on-auto-fill)))
(provide 'init-emacs)
