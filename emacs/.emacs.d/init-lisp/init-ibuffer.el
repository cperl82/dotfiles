(use-package ibuffer
  :straight nil
  :defer t
  :bind
  ([remap list-buffers] . ibuffer))

(use-package ibuffer-vc
  :straight t
  :defer t
  :after ibuffer
  :preface
  (defun cp/ibuffer-mode-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphbaetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  ((ibuffer . cp/ibuffer-mode-setup)))
