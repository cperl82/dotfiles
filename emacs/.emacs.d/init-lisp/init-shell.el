(use-package sh-script
  :defer t
  :preface
  (defun cp/sh-mode-setup ()
    (sh-set-shell "bash")
    (flycheck-mode)
    (flycheck-select-checker 'sh-shellcheck)
    (sh-electric-here-document-mode -1)
    (cp/enable-hideshow-and-hide-all))
  :hook
  ((sh-mode . cp/sh-mode-setup)))
