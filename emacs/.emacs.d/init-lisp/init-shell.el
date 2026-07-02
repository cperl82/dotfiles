(use-package sh-script
  :defer t
  :preface
  (defun cp/sh-mode-setup ()
    (sh-set-shell "bash")
    (flycheck-mode)
    (flycheck-select-checker 'sh-shellcheck)
    (sh-electric-here-document-mode -1))
  :hook
  ((sh-mode . cp/sh-mode-setup)
   (sh-mode . cp/enable-hideshow-and-hide-all)))
