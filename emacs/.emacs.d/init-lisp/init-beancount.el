(use-package beancount
  :straight t
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-beancount))
  (defun cp/beancount-mode-setup ()
    (outline-minor-mode)
    (flycheck-mode)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  :hook
  ((beancount-mode . cp/beancount-mode-setup)))

(use-package flycheck-beancount
  :straight (flycheck-beancount
             :type git
             :host github
             :repo "John-Goff/flycheck-beancount")
  :defer t
  :after (beancount flycheck))
