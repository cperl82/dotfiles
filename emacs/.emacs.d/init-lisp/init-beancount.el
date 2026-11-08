(use-package beancount
  :straight t
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-beancount))
  :hook ((beancount-mode . outline-minor-mode)
         (beancount-mode . flycheck-mode)))

(use-package flycheck-beancount
  :straight (flycheck-beancount
             :type git
             :host github
             :repo "John-Goff/flycheck-beancount")
  :defer t
  :after (beancount flycheck))
