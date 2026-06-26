(use-package emacs
  :demand t
  :custom
  (ad-redefinition-action #'accept)
  (column-number-mode t)
  (confirm-kill-emacs #'yes-or-no-p)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (load-prefer-newer t)
  (make-backup-files nil)
  (use-short-answers t)
  :config
  (when (< emacs-major-version 27)
    (package-initialize))
  (when (> emacs-major-version 28)
    (setq native-comp-async-report-warnings-errors nil))
  (put 'narrow-to-region 'disabled nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-auto-revert-mode)
  :hook
  ((prog-mode . turn-on-auto-fill)))
(provide 'init-emacs)
