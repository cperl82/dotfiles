(use-package general
  :demand t
  :straight t
  :config
  (defconst cp/normal-prefix "SPC")
  (defconst cp/non-normal-prefix "M-SPC")

  ;; Unbind existing keybindings in evil-motion-state-map
  (general-define-key
   :keymaps '(motion)
   "SPC" nil
   ","   nil
   )

  ;; Global keybindings
  (general-define-key
   :keymaps '(override)
   :states '(motion insert emacs)
   "M-o" #'other-window
   )

  (general-define-key
   :keymaps '(override)
   :states '(motion emacs)
   :prefix ","
   "h" #'swiper-isearch-thing-at-point
   "s" #'split-window-vertically
   "v" #'split-window-horizontally
   "x" #'delete-window
   "o" #'delete-other-windows
   "j" #'dired-jump
   "k" #'kill-buffer
   "f" #'find-file
   "r" #'cp/find-file-sudo
   "e" '(:package escreen :keymap escreen-map :which-key "escreen")
   "w o" #'other-window
   "w h" #'windmove-left
   "w j" #'windmove-down
   "w k" #'windmove-up
   "w l" #'windmove-right
   "w u" #'winner-undo
   "w r" #'winner-redo
   "w H" #'windmove-swap-states-left
   "w J" #'windmove-swap-states-down
   "w K" #'windmove-swap-states-up
   "w L" #'windmove-swap-states-right
   "w R" #'cp/hydra-windsize/body
   )
  )

(provide 'init-general)
