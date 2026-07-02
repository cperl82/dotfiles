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
  ;; Global overrides
  (:keymaps '(override)
   :states '(motion insert emacs)
   "M-o" #'other-window
   "C-s" #'swiper-isearch)
  ;; , leader
  (:keymaps '(override)
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
   "w R" #'cp/hydra-windsize/body)
  ;; SPC leader
  (:keymaps '(override)
   :states '(motion emacs)
   :prefix "SPC"
   "a c" '(:package xcscope :keymap cscope-command-map :which-key "xcscope")
   "a p" '(:package projectile :keymap projectile-command-map :which-key "projectile")
   "a g r" #'cp/counsel-rg
   "a g f" #'cp/counsel-rg-files)
  ;; visual tab indent
  (:states '(visual) "TAB" #'indent-region)
  ;; hideshow
  (:states '(normal)
   :keymaps '(hs-minor-mode-map)
   "TAB" #'hs-toggle-hiding)
  ;; org
  (:keymaps '(org-mode-map)
   :states  '(motion)
   "TAB"     #'org-cycle
   "M-h"     #'org-metaleft
   "M-j"     #'org-metadown
   "M-k"     #'org-metaup
   "M-l"     #'org-metaright
   "M-H"     #'org-shiftmetaleft
   "M-J"     #'org-shiftmetadown
   "M-K"     #'org-shiftmetaup
   "M-L"     #'org-shiftmetaright
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
  (:keymaps '(org-mode-map)
   :states  '(motion)
   :prefix cp/normal-prefix
   "o"       '(:ignore t :which-key "org")
   "o p"     #'org-previous-link
   "o n"     #'org-next-link
   "o a"     #'org-agenda
   "o t"     #'org-todo
   "o T"     #'org-set-tags
   "o P"     #'org-set-property
   "o s"     #'cp/org-sort-entries
   "o h"     #'org-metaleft
   "o j"     #'org-metadown
   "o k"     #'org-metaup
   "o l"     #'org-metaright
   "o H"     #'org-shiftmetaleft
   "o J"     #'org-shiftmetadown
   "o K"     #'org-shiftmetaup
   "o L"     #'org-shiftmetaright
   "o c"     #'cp/org-save-all-org-buffers-and-commit)
  (:keymaps '(org-mode-map)
   :states  '(insert)
   "M-RET"   #'org-meta-return
   "M-."     #'cp/org-surround-tilda
   "M-,"     #'cp/org-surround-equal)
  (:keymaps '(org-agenda-mode-map)
   :states  '(motion)
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
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
  (setq evil-normal-state-tag   " N"
        evil-insert-state-tag   " I"
        evil-visual-state-tag   " V"
        evil-replace-state-tag  " R"
        evil-motion-state-tag   " M"
        evil-operator-state-tag " O"
        evil-emacs-state-tag    " E")
  (evil-mode)
  :hook
  ;; CR-someday cperl: I'm not entirely sure why this is necessary, but without
  ;; it, the edebug map doesn't get its proper position as an "intercept" map,
  ;; which makes edebug really annoying to use
  ((edebug-mode . evil-normalize-keymaps)))

(use-package evil-collection
  :straight t
  :demand t
  :after evil
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (defun cp/evil-collection-setup (&rest _)
    ;; org-agenda-mode evil-collection overrides
    (evil-collection-define-key 'normal 'org-agenda-mode-map
      "S" #'cp/org-save-all-org-buffers-and-commit))
  (evil-collection-init)
  :hook
  ((evil-collection-setup . cp/evil-collection-setup)))

(use-package evil-surround
  :straight t
  :demand t
  :after evil
  :preface
  (defun cp/evil-surround-add-org-mode-pairs ()
    (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
    (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)
    (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
  (defun cp/evil-surround-add-elisp-mode-pairs ()
    (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
  :config
  (global-evil-surround-mode)
  :hook
  ((org-mode . cp/evil-surround-add-org-mode-pairs)
   (emacs-lisp-mode . cp/evil-surround-add-elisp-mode-pairs)))

(use-package evil-smartparens
  :straight t
  :demand t
  :after (evil)
  :preface
  (defun cp/smartparens-setup ()
    (smartparens-mode)
    (smartparens-strict-mode)
    (evil-smartparens-mode))
  :hook
  ((lisp-mode . cp/smartparens-setup)
   (emacs-lisp-mode . cp/smartparens-setup))
  :config
  (sp-local-pair
   '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "'" nil :actions nil)
  (sp-local-pair
   '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "`" nil :actions nil))
