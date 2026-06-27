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
  (:keymaps '(visual) "TAB" #'indent-region)
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
