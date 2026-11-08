(use-package beancount
  :straight t
  :defer t
  :hook ((beancount-mode . outline-minor-mode)
         (beancount-mode . flycheck-mode)))

(use-package flycheck-beancount
  :straight (flycheck-beancount
             :type git
             :host github
             :repo "John-Goff/flycheck-beancount")
  :init
  (with-eval-after-load 'flycheck
    (require 'flycheck-beancount))
  :defer t
  :after (beancount flycheck))
