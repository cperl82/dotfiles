;; http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(use-package undo-tree
  :straight t
  :demand t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package evil
  :straight t
  :demand t
  :general
  ;; Unbind existing keybindings in evil-motion-state-map
  (:states '(motion) "SPC" nil "," nil)
  (:states '(motion insert emacs)
   :keymaps '(override)
   "M-o" #'other-window)
  (:states '(motion emacs)
   :keymaps '(override)
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
   "w R" #'cp/hydra-windsize/body)
  (:states '(motion emacs)
   :keymaps '(override)
   :prefix "SPC"
   "a c" '(:package xcscope :keymap cscope-command-map :which-key "xcscope")
   "a g r" #'cp/counsel-rg
   "a g f" #'cp/counsel-rg-files)
  (:states '(normal)
   :keymaps '(hs-minor-mode-map)
   "TAB" #'hs-toggle-hiding)
  (:states '(visual) "TAB" #'indent-region)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-respect-visual-line-mode t)
  (evil-flash-delay 5)
  (evil-move-beyond-eol t)
  (evil-symbol-word-search t)
  (evil-mode-line-format '(before . mode-line-mule-info))
  :config
  (setq evil-normal-state-tag   " N")
  (setq evil-insert-state-tag   " I")
  (setq evil-visual-state-tag   " V")
  (setq evil-replace-state-tag  " R")
  (setq evil-motion-state-tag   " M")
  (setq evil-operator-state-tag " O")
  (setq evil-emacs-state-tag    " E")
  (evil-mode)
  (evil-select-search-module 'evil-search-module 'isearch)
  :hook
  ;; CR-someday cperl: I'm not entirely sure why this is necessary, but without
  ;; it, the edebug map doesn't get its proper position as an "intercept" map,
  ;; which makes edebug really annoying to use
  ((edebug-mode . evil-normalize-keymaps)))

(use-package evil-collection
  :straight t
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :demand t
  :after evil
  :config
  (global-evil-surround-mode))

(provide 'init-evil)
