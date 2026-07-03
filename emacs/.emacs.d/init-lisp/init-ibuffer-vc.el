(use-package ibuffer-vc
  :straight t
  :defer t
  :after ibuffer
  :preface
  (defun cp/ibuffer-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphbaetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  ((ibuffer . cp/ibuffer-setup))
  :general
  (:keymaps '(ibuffer-mode-map)
   :states  '(emacs)
   "l" #'ibuffer-visit-buffer
   "j" #'evil-next-line
   "k" #'evil-previous-line
   "r" #'ibuffer-update
   )
  )
