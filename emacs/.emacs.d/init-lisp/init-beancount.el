(use-package beancount
  :straight t
  :defer t
  :config
  (with-eval-after-load 'flycheck
    ;; 2026-11-11 cperl: Inlined from:
    ;; https://github.com/John-Goff/flycheck-beancount as I want to
    ;; use `source-inplace' rather than `source' so include directives
    ;; work properly
    (flycheck-define-checker beancount
      "A beancount syntax checker using bean-check."
      :command ("bean-check" source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ": " (message) line-end))
      :modes beancount-mode)
    (add-to-list 'flycheck-checkers 'beancount))
  (defun cp/beancount-mode-setup ()
    (outline-minor-mode)
    (flycheck-mode)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  :hook
  ((beancount-mode . cp/beancount-mode-setup)))
