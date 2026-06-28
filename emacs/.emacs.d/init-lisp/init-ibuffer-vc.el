(use-package ibuffer-vc
  :defer t
  :straight t
  :after ibuffer
  :preface
  (defun cp/ibuffer-hook-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphbaetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  ((ibuffer . cp/ibuffer-hook-setup))
  :general
  (:keymaps '(ibuffer-mode-map)
   :states  '(emacs)
   "l" #'ibuffer-visit-buffer
   "j" #'evil-next-line
   "k" #'evil-previous-line
   "r" #'ibuffer-update
   )
  )
