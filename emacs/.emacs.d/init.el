;; -*- lexical-binding: t -*-
(when (< emacs-major-version 27)
  (package-initialize))

(when (> emacs-major-version 28)
  (setq native-comp-async-report-warnings-errors nil))

;; 2020-10-22 emacs startup tweaks from https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(setq gc-cons-threshold (* 100 1000 1000))

;; straight
(setq straight-cache-autoloads t
      straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(dolist
    (package
      '(avy
        ace-window
        adaptive-wrap
        annalist
        color-theme-zenburn
        company-mode
        compat
        counsel-projectile
        dash
        diminish
        elisp-slime-nav
        embark
        evil
        evil-collection
        evil-smartparens
        evil-surround
        f
        flycheck
        general
        go-mode
        haskell-mode
        highlight-parentheses
        hydra
        ibuffer-vc
        ibuffer-projectile
        ios-config-mode
        ivy-rich
        json-mode
        json-reformat
        json-snatcher
        lua-mode
        magit
        markdown-mode
        nasm-mode
        nerd-icons
        ob-async
        org
        org-ql
        origami
        projectile
        rust-mode
        s
        smartparens
        doom-modeline
        smex
        swiper
        systemd-mode
        systemtap-mode
        tuareg-mode
        undo-tree
        unfill
        use-package
        vagrant-tramp
        visual-fill-column
        wgrep
        which-key
        windsize
        xcscope
        yaml-mode))
  (straight-use-package package))


;; Base packages
(require 'general)
(require 'use-package)
(require 'dash)
(require 's)
(require 'f)


;; Misc
(setq ad-redefinition-action    'accept
      c-default-style           "linux"
      column-number-mode        t
      confirm-kill-emacs        'yes-or-no-p
      create-lockfiles          nil
      enable-recursive-minibuffers t
      indent-tabs-mode          nil
      inhibit-startup-message   t
      load-prefer-newer         t
      make-backup-files         nil
      split-height-threshold    nil
      window-combination-resize t)

;; 2021-10-22 Always use the short form
(defalias 'yes-or-no-p 'y-or-n-p)

;; 2015-09-11 Enable narrowing command which are disabled by default
(put 'narrow-to-region 'disabled nil)

;; 2018-10-30 Global auto revert mode
(global-auto-revert-mode)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defmacro cp/make-symbol-caching-version-of (name f timeout)
  `(progn
     (fset
      ,name
      (let
          ((expire ,timeout)
           (cache (make-hash-table :test 'equal)))
        (lambda (&rest args)
          (let* ((cache-entry (gethash args cache))
                 (now         (current-time)))
            (if (or current-prefix-arg
                    (not cache-entry)
                    (> (time-to-seconds (time-subtract now (car cache-entry))) expire))
                (progn
                  (message "Regenerating cache for %S with args %S" ,name args)
                  (let ((result (apply ,f args)))
                    (puthash args (cons now result) cache)
                    result))
              (cdr cache-entry))))))
     ,name))

(defun cp/revert-buffer-all ()
  "Revert all buffers.  This reverts buffers that are visiting a file, kills
buffers whose visited file has disappeared and refreshes dired buffers."
  (interactive)
  (if (y-or-n-p "Revert ALL buffers? ")
      (save-excursion
        (dolist (b (buffer-list))
          (set-buffer b)
          (cond
           (buffer-file-name
            (if (file-exists-p buffer-file-name)
                (revert-buffer t t t)
              (kill-buffer b)))
           ((eq major-mode 'dired-mode) (revert-buffer t t t)))))))

(defun cp/advice/split-windows (&rest r)
  "Try to always maintain two windows, side by side (i.e. split
horizontally), unless we get below some absolute minimum

The idea is to keep the `split-width-threshold' set to the number of
columns in the frame.  That way, as soon as more than one window has
been created, any given window no longer meets the criteria for being
able to be split again, even if the windows are unbalanced (e.g. one
is only a few columns wide and the other takes up all the remaining
space)"
  (let ((width (frame-text-width)))
    (if (< width 120)
        (setq split-width-threshold nil)
      (when (not (equal width split-width-threshold))
        (setq split-width-threshold width)))))

(advice-add 'split-window-sensibly     :before #'cp/advice/split-windows)
(advice-add 'split-window-horizontally :before #'cp/advice/split-windows)

;; 2017-08-17 cperl: misc stuff
(defun cp/c-mode-hook-setup ()
  (hs-minor-mode))
(add-hook 'c-mode-hook #'cp/c-mode-hook-setup)

(defun cp/find-file ()
  "A wrapper for `find-file'"
  (interactive)
  ;; Explicitly call `counsel-find-file' if in `counsel-mode'
  ;; as it has additional actions and such that `find-file'
  ;; does not, even though they look basically the same from
  ;; a ui perspective.
  (let ((f (if counsel-mode #'counsel-find-file #'find-file))
        (default-directory
         (if current-prefix-arg
             (let* ((buffers
                     (-filter
                      (lambda (b)
                        (let ((name (buffer-name b)))
                          (or (not (string-match "^\\(  *\\)?\\*.*\\*$" name))
                              (equal "*scratch*" name))))
                      (buffer-list)))
                    (directories
                     (-map
                      (lambda (b)
                        (with-current-buffer b (abbreviate-file-name default-directory)))
                      buffers))
                    (cands (-sort #'string-equal (-uniq directories))))
               (ivy-read "Find file in directory: " cands))
           (if (eq major-mode 'dired-mode) (dired-current-directory) default-directory))))
    (call-interactively f)))

;; Adapted from https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun cp/find-file-sudo ()
    "Edit a file as root. "
    (interactive)
    (find-file (concat "/sudo:root@localhost:"
                       (read-file-name "Find file (as root): "))))

(defun cp/run-counsel-grep-or-swiper-with-thing-at-point ()
  (interactive)
  (let ((initial-input
         (evil-find-thing t 'symbol)))
    (counsel-grep-or-swiper initial-input)))

(defconst cp/normal-prefix "SPC")
(defconst cp/non-normal-prefix "M-SPC")

;; Unbind existing keybindings in evil-motion-state-map
(general-define-key
 :keymaps '(motion)
  "SPC" nil
  ","   nil)

;; Global keybindings
(general-define-key
 :keymaps '(override)
 :states '(normal motion emacs)
 :prefix ","
 "h" #'swiper-isearch-thing-at-point
 "m" #'highlight-symbol-at-point
 "u" #'unhighlight-regexp
 "s" #'split-window-vertically
 "v" #'split-window-horizontally
 "x" #'delete-window
 "o" #'delete-other-windows
 "j" #'dired-jump
 "k" #'kill-buffer
 "f" #'cp/find-file
 "u" #'cp/find-file-sudo
 "r" #'counsel-recentf
 "R" #'projectile-recentf)

(general-define-key
 :keymaps '(override)
 :states '(normal motion insert emacs)
 :prefix cp/normal-prefix
 :non-normal-prefix cp/non-normal-prefix
 "a"   '(:ignore t :which-key "applications")
 "b"   '(:ignore t :which-key "buffers")
 "w"   '(:ignore t :which-key "windows")
 "f"   '(:ignore t :which-key "files")
 "h"   #'help
 "f f" #'cp/find-file
 "f r" #'find-file-read-only
 "f j" #'dired-jump
 "b b" #'switch-to-buffer
 "b k" #'kill-buffer
 "b K" #'kill-buffer-and-window
 "b r" #'revert-buffer
 "b R" #'cp/revert-buffer-all
 "b f" #'(lambda () (interactive) (message (buffer-file-name)))
 "w s" #'split-window-vertically
 "w v" #'split-window-horizontally
 "w K" #'kill-buffer-and-window
 "w o" #'delete-other-windows
 "w x" #'delete-window
 "w =" #'balance-windows)

(general-define-key
 :keymaps '(override)
 :states '(normal motion insert emacs)
 "C-h" #'windmove-left
 "C-j" #'windmove-down
 "C-k" #'windmove-up
 "C-l" #'windmove-right)


;; zenburn
(load-theme 'zenburn t)



;; which-key
(use-package which-key
  :defer 5
  :diminish which-key-mode
  :config
  (progn
    (setq which-key-idle-delay 1.0)
    (which-key-mode)))



;; buffer-move
(use-package buffer-move
  :defer t
  :commands (buf-move-down buf-move-up buf-move-left buf-move-right)
  :load-path "lisp"
  :general
  (:keymaps '(override)
   :states '(normal motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w H" #'buf-move-left
   "w J" #'buf-move-down
   "w K" #'buf-move-up
   "w L" #'buf-move-right))



;; edebug
(use-package edebug
  :defer t
  :init
  ;; CR-someday cperl: I'm not entirely sure why this is necessary, but without
  ;; it, the edebug map doesn't get its proper position as an "intercept" map,
  ;; which makes edebug really annoying to use
  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps))



;; company-mode
(use-package company-mode
  :defer t
  :diminish company-mode
  :general
  (:keymaps '(company-active-map)
   "M-n" nil
   "M-p" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous))



;; avy
(use-package avy
  :defer t
  :general
  (:keymaps '(override)
   :states '(normal motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a a"   '(:ignore t :which-key "avy")
   "a a c" #'avy-goto-char
   "a a C" #'avy-goto-char-2
   "a a w" #'avy-goto-word-1)
  :config
  (progn
    (setq avy-background t)))

;; ace-window
(use-package ace-window
  :defer t
  :general
  (:keymaps '(override)
   :states '(normal motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w a" #'ace-window)
  (:keymaps '(override)
   :states '(normal motion insert emacs)
   "M-o" #'ace-window)
  :init
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))))



;; paren
(use-package paren
  :init
  (progn
    (setq show-paren-delay 0))
  :config
  (progn
    (show-paren-mode)))



;; uniquify
(use-package uniquify
  :defer t
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward
          uniquify-separator " @"
          uniquify-strip-common-suffix nil
          uniquify-after-kill-buffer-p t)))


;; swiper / ivy / counsel / smex
(defun cp/counsel-rg-type-list ()
  (thread-last (shell-command-to-string "rg --type-list")
    (s-split "\n")))

(defun cp/counsel-rg (&rest args)
  "A wrapper around `counsel-rg' that increases the level of the
prefix argument (from 0 to 1 or from 1 to 2).  The point is to always
call `counsel-rg' with a prefix argument to force it to prompt for a
directory, but optionally allow a single prefix argument can force it
to prompt for additional args"
  (interactive)
  (let ((current-prefix-arg
         (cond
           ((equal current-prefix-arg nil) '(4))
           ((equal current-prefix-arg '(4)) '(16))
           (t current-prefix-arg))))
    (apply #'counsel-rg args)))

(defun cp/counsel-rg-with-type (&optional types prompt)
  "Prompt for a supported file type from rg and then run
`counsel-rg' as if a prefix arg was passed, but explicitly
setting the args to `-t TYPE' instead of prompting."
  (interactive)
  (let* ((file-types
          (or types
              (thread-last (ivy-read "File type: " (cp/counsel-rg-type-list) :require-match t)
                (s-split ":")
                (nth 0)
                (list))))
         (extra-rg-args
          (s-join " " (seq-map (lambda (type) (format "-t%s" type)) file-types))))
    (cp/counsel-rg nil nil extra-rg-args prompt)))

(defun cp/counsel-rg-with-type-ocaml ()
  (interactive)
  (cp/counsel-rg-with-type '("ocaml") "rg (ocaml): "))

(defun cp/counsel-rg-with-type-c ()
  (interactive)
  (cp/counsel-rg-with-type '("c") "rg (c): "))

(defun cp/counsel-rg-with-type-ocaml-or-c ()
  (interactive)
  (cp/counsel-rg-with-type '("ocaml" "c") "rg (ocaml or c): "))

(defun cp/counsel-rg-with-type-elisp ()
  (interactive)
  (cp/counsel-rg-with-type '("elisp") "rg (elisp): "))

(defun cp/counsel-rg-with-type-lisp ()
  (interactive)
  (cp/counsel-rg-with-type '("lisp") "rg (lisp): "))

(defun cp/counsel-rg-files (&optional initial-input)
  "Find files with `rg --files'.  This started off as just setting
`counsel-git-command' and then running `counsel-git', but I had to
give that up as it requires being run from within a git repo, and this
is more general than that.

By default, prompt for a directory to start in.  If passed a prefix
argument, then just use `default-directory'
"
  (interactive)
  (counsel-require-program "rg")
  (let* ((cmd "rg --files --hidden -g '!.git/*' -g '!.hg/*'")
         (default-directory (if current-prefix-arg default-directory (read-directory-name "rg --files in directory: ")))
         (cands (split-string (shell-command-to-string cmd) "\n" t)))
    (ivy-read "Find file: " cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'cp/counsel-rg-files)))

(defun cp/counsel-grep-use-swiper-p ()
  "A small wrapper for `counsel-grep-use-swiper-p-default' that
ensures we always use swiper on encrypted buffers/files as
attempting to use grep (or ag, rg, etc) is always going to fail."
  (let ((name (buffer-file-name)))
    (if (and name (string-match-p "\\.gpg$" name))
        t
      (counsel-grep-use-swiper-p-default))))

(use-package smex
  :defer t)

(use-package ivy
  :defer t
  :diminish ivy-mode
  :general
  (:keymaps '(ivy-minibuffer-map)
   "<up>"   #'ivy-previous-history-element
   "<down>" #'ivy-next-history-element)
  (:keymaps '(override)
   "C-s"    #'swiper-isearch)
  :init
  (progn
    (setq ivy-height 10)
    (setq ivy-initial-inputs-alist nil)
    ;; 2017-05-06: `ivy--regex-ignore-order' doesn't seem to work well with swiper as the
    ;; buffer highlights don't seem to be applied correctly.  However, its useful when
    ;; using `counsel-M-x', so enable it for that (e.g. typing `dired find' to find
    ;; `find-dired').
    (setq ivy-re-builders-alist
          '((counsel-M-x . ivy--regex-ignore-order)
            (t . ivy--regex-plus)))
    (setq ivy-count-format "%d/%d ")
    (ivy-mode 1))
  :config
  (progn
    (setf (cdr (assoc t ivy-format-functions-alist)) #'ivy-format-function-arrow))
  )

(use-package ivy-rich
  :after (ivy)
  :config
  (progn
    (let ((switch-buffer-prefs
           '(:columns
             ((ivy-rich-candidate
               (:width 50))
              (ivy-rich-switch-buffer-size
               (:width 7))
              (ivy-rich-switch-buffer-indicators
               (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode
               (:width 20 :face warning))
              (ivy-rich-switch-buffer-project
               (:width 20 :face success))
              (ivy-rich-switch-buffer-path
               (:width
                (lambda (x)
                  (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda
                 (cand)
               (get-buffer cand)))))
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer
                 switch-buffer-prefs)
      (plist-put ivy-rich-display-transformers-list
                 'counsel-projectile-switch-to-buffer
                 switch-buffer-prefs)
      (plist-put ivy-rich-display-transformers-list
                 'counsel-projectile-find-file
                 switch-buffer-prefs)
      (plist-put ivy-rich-display-transformers-list
                 'counsel-projectile-find-dir
                 switch-buffer-prefs)
      (plist-put ivy-rich-display-transformers-list
                 'counsel-projectile
                 switch-buffer-prefs))
    (setq ivy-rich-path-style 'relative)
    (ivy-rich-mode 1)))

(use-package counsel
  :defer t
  :diminish counsel-mode
  :general
  (:keymaps '(override)
   :states '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a g r" #'cp/counsel-rg
   "a g f" #'cp/counsel-rg-files
   "a g R" #'cp/counsel-rg-with-type
   "a g O" #'cp/counsel-rg-with-type-ocaml
   "a g C" #'cp/counsel-rg-with-type-c
   "a g J" #'cp/counsel-rg-with-type-ocaml-or-c
   "a g E" #'cp/counsel-rg-with-type-elisp
   "a g L" #'cp/counsel-rg-with-type-lisp)
  :init
  (progn
    ;; Use rg instead of grep because it has the nice smart case feature
    (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")
    (setq counsel-grep-use-swiper-p #'cp/counsel-grep-use-swiper-p)
    (counsel-mode 1)))



;; dired
(defun cp/dired-tab-dwim ()
  (interactive)
  (if (dired-get-subdir)
      (dired-hide-subdir 1)
    (ignore-errors
      (dired-find-file-other-window))))

(defun cp/dired-toggle-hiding-dotfiles ()
  (interactive)
    (let ((regex "^\\..*$")
          (elements (s-split "\\\\|" dired-omit-files)))
      (if (-contains? elements regex)
          (setq dired-omit-files (s-join "\\|" (-remove-item regex elements)))
        (setq dired-omit-files (s-join "\\|" (-insert-at 0 regex elements))))
      (revert-buffer)
      (message "dired-omit-files is now: %S" dired-omit-files)))

(defun cp/dired-smart-async-shell-command (command &optional output-buffer error-buffer)
  "Like function `async-shell-command', but in the current Virtual
Dired directory.  Copied from `dired-smart-shell-command' from
dired-x"
  (interactive
   (list
    (read-shell-command "Async shell command: " nil nil
			(cond
			 (buffer-file-name (file-relative-name buffer-file-name))
			 ((eq major-mode 'dired-mode) (dired-get-filename t t))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (async-shell-command command output-buffer error-buffer)))

(use-package dired
  :defer t
  :general
  (:keymaps '(dired-mode-map)
   :states  '(normal motion)
   "h"   #'dired-up-directory
   "l"   #'dired-find-file
   "n"   #'evil-search-next
   "N"   #'evil-search-previous
   "?"   #'evil-search-backward
   "G"   #'evil-goto-line
   "gg"  #'evil-goto-first-line
   "M-K" #'dired-kill-subdir
   "M-n" #'dired-next-subdir
   "M-p" #'dired-prev-subdir
   "TAB" #'cp/dired-tab-dwim
   "."   #'cp/dired-toggle-hiding-dotfiles
   "M-&" #'cp/dired-smart-async-shell-command
   ", f" #'cp/dired-smart-find-file)
  :config
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (let ((prog (if (equal system-type 'darwin) "gls" "ls")))
      (setq insert-directory-program prog))
    (setq dired-listing-switches "-aBhl --group-directories-first"
          dired-dwim-target t)
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode 1)))))

(use-package dired-x
  :after (dired))


;; ibuffer-vc
(use-package ibuffer-vc
  :defer t
  :general
  (:keymaps '(ibuffer-mode-map)
   :states  '(emacs)
   "l" #'ibuffer-visit-buffer
   "j" #'evil-next-line
   "k" #'evil-previous-line
   "r" #'ibuffer-update
   )
  :config
  (progn
    (add-hook
     'ibuffer-hook
     (lambda ()
       (ibuffer-vc-set-filter-groups-by-vc-root)
       (unless (eq ibuffer-sorting-mode 'alphbaetic)
         (ibuffer-do-sort-by-alphabetic))))))



;; xcscope
(use-package xcscope
  :defer t
  :general
  (:keymaps '(override)
   :states '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a c" '(:keymap cscope-command-map :which-key "cscope"))
  (:keymaps '(cscope-list-entry-keymap)
   :states  '(normal motion)
   "RET" #'cscope-select-entry-inplace
   "TAB" #'cscope-select-entry-other-window
   "o"   #'cscope-select-entry-other-window
   "C-k" #'cscope-history-kill-file
   "M-q" #'cscope-history-kill-result
   "q"   #'bury-buffer
   "Q"   #'cscope-quit
   "d"   #'cscope-dired-directory
   "U"   #'cscope-unset-initial-directory
   "T"   #'cscope-tell-user-about-directory
   "M-n" #'cscope-history-forward-file
   "M-p" #'cscope-history-backward-file
   "M-N" #'cscope-history-forward-result
   "M-P" #'cscope-history-backward-result)
  :config
  (progn
    (setq cscope-option-use-inverted-index t)
    (setq cscope-edit-single-match nil)
    (setq cscope-option-kernel-mode t)
    (add-hook
     'cscope-list-entry-hook
     (lambda ()
       ;; This should be able to be specified via `evil-set-initial-state', but that
       ;; doesn't seem to work for the cscope buffer as it seems to be in fundamental-mode
       ;; when evil loads for the first time.  I'm not entirely sure what is going on, but
       ;; this works as workaround for now.
       (evil-motion-state)
       (setq-local
        face-remapping-alist
        '((cscope-separator-face   font-lock-string-face)
          (cscope-line-number-face font-lock-string-face)
          (cscope-file-face        font-lock-doc-face)
          (cscope-function-face    font-lock-function-name-face)))))))



;; lisp-mode
(use-package elisp-mode
  :defer t
  :config
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (setq lisp-loop-forms-indentation 3)
     (setq lisp-indent-function #'lisp-indent-function)
     (setq indent-tabs-mode nil)
     (hs-minor-mode)
     (hs-hide-all)
     (company-mode))))



;; elisp-slime-nav
(use-package elisp-slime-nav
  :defer t
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
  :general
  (:keymaps '(elisp-slime-nav-mode-map)
   :states  '(normal motion)
   "C-c &"   #'evil-jump-backward
   "C-c ;"   #'elisp-slime-nav-find-elisp-thing-at-point
   "C-c C-t" #'elisp-slime-nav-describe-elisp-thing-at-point))



;; tuareg-mode
(defun cp/tuareg-mode-hs-forward-sexp-fun (arg)
  (let* ((c (current-column))
         (re (format "^[[:space:]]\\{,%d\\}[^[:space:]]\\|\\'" c)))
    (forward-line 1)
    (re-search-forward re)
    (beginning-of-line)
    (back-to-indentation)
    (if (not
         (or
          (looking-at "in")
          (looking-at "end")
          (looking-at ";;")
          (looking-at "with")))
        (progn
          (forward-line -1)
          (move-end-of-line nil)))))

;; 2016-03-21: Alternative implementation that deals with functions without ;; better
(defun cp/tuareg-mode-hs-forward-sexp-fun-alt (arg)
  (let* ((c (current-column))
         (re (format "^[[:space:]]\\{,%d\\}[^[:space:]]\\|\\'" c)))
    (forward-line 1)
    (re-search-forward re)
    (beginning-of-line)
    (back-to-indentation)
    (if (not
         (or
          (looking-at "in")
          (looking-at "end")
          (looking-at ";;")
          (looking-at "with")
       (eq (point) (buffer-size))))
        (progn
          (re-search-backward "^[:space:]*[^:space:].")
          (move-end-of-line nil)))))

(setq cp/tuareg-mode-hs-start-regexp
      (mapconcat
       'identity
       '("\\<module\\>\\s-+\\S-+\\s-+=\\s-+"
         "\\<module\\>\\s-+\\S-+\\s-+:\\s-+\\<sig\\>"
         "\\<module\\>\\s-+\\<type\\>\\s-+\\S-+\\s-+=\\s-+\\<sig\\>"
         "\\<end\\>\\s-+=\\s-+\\<struct\\>"
         "\\<let\\>\\s-+"
         "\\<and\\>\\s-+"
         "\\<let%\\S-+\\>\\s-+"
         "\\<type\\>\\(\\s-+\\S-+\\)+?\\s-+="
         "\\<TEST_MODULE\\>\\s-+\\S-+\\s-+=\\s-+\\<struct\\>"
         "\\<TEST_UNIT\\>\\s-+="
         )
       "\\|"))

(use-package tuareg
  :defer t
  :config
  (progn
    (add-to-list
     'hs-special-modes-alist
     `(tuareg-mode
       ,cp/tuareg-mode-hs-start-regexp nil nil  cp/tuareg-mode-hs-forward-sexp-fun))
    (add-hook
     'tuareg-mode-hook
     (lambda ()
       (hs-minor-mode)
       (setq-local
        face-remapping-alist
        '((tuareg-font-double-colon-face        tuareg-font-lock-governing-face)
          (tuareg-font-lock-extension-node-face tuareg-font-lock-governing-face)
          (tuareg-font-lock-attribute-face      tuareg-font-lock-governing-face)))))))



;; hideshow
(use-package hideshow
  :defer t
  :after (evil)
  :config
  (progn
    (setq hs-isearch-open t)
    (evil-define-minor-mode-key 'normal 'hs-minor-mode
      (kbd "TAB") #'hs-toggle-hiding)))



;; sh-script
; 2014-12-07 Trying to make sh-mode indentation better
(defun cp/sh-switch-to-indentation (n)
  (interactive "p")
  (progn
    (setq sh-basic-offset n)))

(use-package sh-script
  :defer t
  :config
  (progn
    (add-hook
     'sh-mode-hook
     (lambda ()
       (sh-set-shell "bash")
       (flycheck-mode)
       (flycheck-select-checker 'sh-shellcheck)))))



;; grep
(use-package grep
  :defer t
  :general
  (:keymaps '(grep-mode-map)
   "SPC" nil)
  :config
  (progn
    (setq grep-find-use-xargs 'gnu)
    (add-to-list 'grep-files-aliases '("ml"  . "*.ml *.mli"))
    (add-to-list 'grep-files-aliases '("mlc" . "*.ml *.mli *.c *.h"))))



;; man
(defun cp/man-forward-sexp-fun (arg)
    (let ((p (point)))
      (forward-line 1)
      (re-search-forward
       (concat Man-heading-regexp "\\|" "\\'" "\\|" "^[^[:space:]]"))
      (beginning-of-line)
      (forward-line -1)
      (if (equal p (point)) (end-of-line))))

(use-package man
  :defer t
  :general
  (:keymaps '(Man-mode-map)
   :states  '(normal motion)
   "TAB" #'hs-toggle-hiding)
  :config
  (progn
    (add-to-list
     'hs-special-modes-alist
     `(Man-mode ,Man-heading-regexp nil nil cp/man-forward-sexp-fun))
    (add-hook
     'Man-mode-hook
     (lambda ()
       (setq-local comment-start "$^")
       (setq-local comment-end   "$^")
       (hs-minor-mode 1)
       (hs-hide-all)
       (goto-char (point-min))
       (re-search-forward "NAME" nil t)
       (hs-show-block)
       (re-search-forward "SYNOPSIS" nil t)
       (hs-show-block)
       (re-search-forward "DESCRIPTION" nil t)
       (hs-show-block)
       (font-lock-add-keywords
        nil          ; Copied from /usr/share/vim/vim74/syntax/man.vim
        `((,Man-heading-regexp          . font-lock-comment-face)
          ("^\\s-*[+-][a-zA-Z0-9]\\S-*" . font-lock-function-name-face)
          ("^\\s-*--[a-zA-Z0-9-]\\S-*"  . font-lock-function-name-face))
        'set)
       (font-lock-mode 1)))))


;; evil
(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-auto-save-history nil)
    (global-undo-tree-mode)))

(use-package evil
  :demand t
  :general
  (:keymaps '(override)
   :states  '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w h" #'evil-window-left
   "w j" #'evil-window-down
   "w k" #'evil-window-up
   "w l" #'evil-window-right)
  (:keymaps '(visual)
   "TAB" #'indent-region)
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-want-integration t)
    (setq evil-want-C-i-jump nil))
  :config
  (progn
    (evil-mode 1)
    (evil-select-search-module 'evil-search-module 'isearch)
    (setq evil-flash-delay 5)
    (setq evil-move-beyond-eol t)
    (setq evil-symbol-word-search t)
    (setq evil-mode-line-format '(before . mode-line-mule-info))
    (zenburn-with-color-variables
      (setq
       evil-normal-state-tag   " N"
       evil-insert-state-tag   " I"
       evil-visual-state-tag   " V"
       evil-replace-state-tag  " R"
       evil-motion-state-tag   " M"
       evil-operator-state-tag " O"
       evil-emacs-state-tag    " E"))))



;; evil-collection
(use-package evil-collection
    :after (evil)
    :diminish evil-collection-unimpaired-mode
    :config
    (evil-collection-init))



;; smartparens/evil-smartparens
; consider stealing some keybindings from https://github.com/expez/evil-smartparens/issues/19
(defun cp/enable-evil-smartparens ()
  (progn
    (smartparens-mode)
    (smartparens-strict-mode)
    (evil-smartparens-mode)))

(use-package evil-smartparens
    :after (evil)
    :diminish evil-smartparens-mode
    :config
    (progn
      (add-hook       'lisp-mode-hook #'cp/enable-evil-smartparens)
      (add-hook 'emacs-lisp-mode-hook #'cp/enable-evil-smartparens)
      (sp-local-pair
       '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "'" nil :actions nil)
      (sp-local-pair
       '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "`" nil :actions nil)))



;; doom-modeline
(use-package doom-modeline
    :after (evil)
    :init
    (progn
      (setq doom-modeline-icon nil)
      (doom-modeline-mode 1)
      (zenburn-with-color-variables
        (set-face-attribute 'doom-modeline-evil-visual-state nil :foreground zenburn-yellow)
        (set-face-attribute 'doom-modeline-evil-replace-state nil :foreground zenburn-orange)
        (set-face-attribute 'doom-modeline-evil-motion-state nil :foreground zenburn-cyan)
        (set-face-attribute 'doom-modeline-evil-operator-state nil :foreground zenburn-blue)
        (set-face-attribute 'doom-modeline-evil-emacs-state nil :foreground zenburn-magenta))))


;; evil-surround
(use-package evil-surround
  :after (evil)
  :config
  (progn
    (global-evil-surround-mode 1)))



;; highlight-parentheses
(use-package highlight-parentheses
  :defer t
  :config
  (progn
    (zenburn-with-color-variables
      (setq hl-paren-colors
            `(,zenburn-red-4 ,zenburn-green ,zenburn-yellow-2 ,zenburn-blue+1)))))



;; windsize
(defhydra cp/hydra-windsize (nil nil)
  "resize"
  ("h" windsize-left  "left")
  ("j" windsize-down  "down")
  ("k" windsize-up    "up")
  ("l" windsize-right "right"))

(use-package windsize
  :defer t
  :general
  (:keymaps '(override)
   :states '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w r" #'cp/hydra-windsize/body))



;; winner
(use-package winner
  :defer t
  :general
  (:keymaps '(override)
   :states  '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w u" #'winner-undo
   "w U" #'winner-redo)
  :init
  (winner-mode))



;; hippie-expand
(use-package hippie-exp
  :defer t
  :init
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))
    (global-set-key [remap dabbrev-expand] 'hippie-expand)))


;; escreen
(defun cp/escreen-swap-screen (a &optional b)
  (when (and (numberp a) (numberp b))
    (let* ((current-screen-number (escreen-get-current-screen-number))
           (b (or b current-screen-number))
           (screen-data-a (escreen-configuration-escreen a))
           (screen-data-b (escreen-configuration-escreen b)))
      (when
          (and
           (not (equal a b))
           (>= a 0) (<= a escreen-highest-screen-number-used)
           (>= b 0) (<= b escreen-highest-screen-number-used))
        (cond ((and screen-data-a screen-data-b)
               ;; Both screens exist
               (setcar screen-data-a b)
               (setcar screen-data-b a))
              ((and screen-data-a (not screen-data-b))
               ;; The other screen doesn't exist
               (setcar screen-data-a b)))
        (cond ((equal current-screen-number a) (setq escreen-current-screen-number b))
              ((equal current-screen-number b) (setq escreen-current-screen-number a))
              (t t))))))

(defun cp/escreen-move-screen (direction screen-number n)
  (let* ((screen-to-move (or screen-number (escreen-get-current-screen-number)))
         (amount-to-move (or n 1))
         (other-screen-number
          (cond ((eq direction 'left)  (- screen-to-move amount-to-move))
                ((eq direction 'right) (+ screen-to-move amount-to-move)))))
    (cond ((and
            (>= other-screen-number 0)
            (<= other-screen-number escreen-highest-screen-number-used))
           (cp/escreen-swap-screen screen-to-move other-screen-number))
          ;; These are the cases where we're moving right off the
          ;; right end or left off the left end
          ((< other-screen-number 0)
           (let ((other-screen-number (+ escreen-highest-screen-number-used other-screen-number)))
             (cp/escreen-swap-screen screen-to-move other-screen-number)))
          ((> other-screen-number escreen-highest-screen-number-used)
           (let ((other-screen-number (- other-screen-number (1+ escreen-highest-screen-number-used))))
             (cp/escreen-swap-screen screen-to-move other-screen-number)))))
  (cp/escreen-show-active-screens))

(defun cp/escreen-move-screen-left (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'left screen-number n))

(defun cp/escreen-move-screen-right (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'right screen-number n))

(defun cp/escreen-rename-screen (&optional name number suppress-message)
  (interactive "sNew screen name: ")
  (let ((screen-data
         (escreen-configuration-escreen (or number escreen-current-screen-number)))
        (new-name (cond ((equal name "") nil)
                        ((stringp name) name)
                        (t "default"))))
    (setcar (cdr screen-data) new-name)
    (when (not suppress-message)
      (cp/escreen-show-active-screens))))

(defun cp/escreen-propertize-screen-number (number)
  (let ((star (propertize "*" 'face 'font-lock-string-face)))
    (cond ((eq escreen-current-screen-number number) (format "%d%s" number star))
          (t (format "%d-" number)))))

(defvar cp/escreen-show-active-screens-fun #'cp/escreen-show-active-screens-auto)

(defun cp/escreen-set-show-active-screens-fun-gen (how)
  (let ((val
         (cond
           ((eq how 'horizontal) #'cp/escreen-show-active-screens-horizontal)
           ((eq how 'vertical) #'cp/escreen-show-active-screens-vertical)
           (t #'cp/escreen-show-active-screens-auto))))
    (setq cp/escreen-show-active-screens-fun val)))

(defun cp/escreen-set-show-active-screens-fun-horizontal ()
  (interactive)
  (message "Setting escreen to always display horizontal")
  (cp/escreen-set-show-active-screens-fun-gen 'horizontal))

(defun cp/escreen-set-show-active-screens-fun-vertical ()
  (interactive)
  (message "Setting escreen to always display vertical")
  (cp/escreen-set-show-active-screens-fun-gen 'vertical))

(defun cp/escreen-set-show-active-screens-fun-auto ()
  (interactive)
  (message "Setting escreen to pick horizontal/vertical automatically")
  (cp/escreen-set-show-active-screens-fun-gen t))

(defvar cp/escreen-show-active-screens-clear-timer nil)

(defun cp/escreen-show-active-screens-gen (how &optional vertical-clear-delay)
  (when cp/escreen-show-active-screens-clear-timer
    (cancel-timer cp/escreen-show-active-screens-clear-timer))
  (let* ((delayed-clear
          (lambda ()
            (run-with-timer (or vertical-clear-delay 2) nil (lambda () (message nil)))))
         (format-screens
          (lambda (format-str join-str)
            (->>
             (cp/escreen-configuration-screen-numbers-and-names)
             (-sort (lambda (s1 s2) (< (car s1) (car s2))))
             (-map
              (lambda (screen)
                (let ((number (car screen))
                      (name   (cdr screen)))
                  (format format-str (cp/escreen-propertize-screen-number number) name))))
             (s-join join-str))))
         (h-args '("%s %s" "  "))
         (v-args
          (list (if (<= (length escreen-configuration-alist) 10) "%2s %s" "%3s %s") "\n"))
         (string
          (cond
            ((eq how 'horizontal) (apply format-screens h-args))
            ((eq how 'vertical)
             (progn
               (setq cp/escreen-show-active-screens-clear-timer (apply delayed-clear ()))
               (apply format-screens v-args)))
            (t
             (let ((horizontal (apply format-screens h-args)))
               (if (< (length horizontal) (frame-width))
                   horizontal
                 (progn
                   (setq cp/escreen-show-active-screens-clear-timer (apply delayed-clear ()))
                   (apply format-screens v-args))))))))
    (message "%s" string))
  nil)

(defun cp/escreen-show-active-screens-horizontal ()
  (interactive)
  (cp/escreen-show-active-screens-gen 'horizontal))

(defun cp/escreen-show-active-screens-vertical ()
  (interactive)
  (cp/escreen-show-active-screens-gen 'vertical))

(defun cp/escreen-show-active-screens-auto ()
  (interactive)
  (cp/escreen-show-active-screens-gen t))

(defun cp/escreen-show-active-screens ()
  (interactive)
  (funcall cp/escreen-show-active-screens-fun))

(defun cp/escreen-configuration-screen-numbers-and-names ()
  (-map
   (lambda (entry) `(,(nth 0 entry) . ,(nth 1 entry)))
   escreen-configuration-alist))

(defun cp/escreen-ivy-screen-number-to-datum (width n)
  ;; CR-soon cperl: This could use some cleaning and rethinking of the interface
  (let* ((screen-data (escreen-configuration-escreen n))
         (name (escreen-configuration-screen-name screen-data))
         (n-windows (length (escreen-configuration-data-map screen-data)))
         (s
          (if width
              (let* ((number (cp/escreen-propertize-screen-number n))
                     (fmt (format
                           (concat
                            (if (<= (length escreen-configuration-alist) 10) "%2s" "%3s")
                            " "
                            "%%-%ds"
                            " "
                            "(%%d buffers)")
                           number
                           width))
                     (n-buffers
                      (->> screen-data
                           (nth 3)
                           (--map (nth 0 it))
                           (--map (nth 1 it))
                           (length))))
                (format fmt name n-buffers))
            (format "%d:%s" n name))))
    `(,s . ,n)))

(defun cp/escreen-ivy-collection ()
  (let* ((current (escreen-get-current-screen-number))
         (numbers-and-names (cp/escreen-configuration-screen-numbers-and-names))
         (all-but-current (-filter (lambda (x) (not (equal (car x) current))) numbers-and-names))
         (width (-reduce-from (lambda (a b) (max a (length (cdr b)))) 0 all-but-current)))
    (->>
     (-map 'car all-but-current)
     (-sort '<)
     (-map (lambda (n) (cp/escreen-ivy-screen-number-to-datum width n))))))

(defun cp/escreen-ivy-action (selected)
  (escreen-goto-screen (cdr selected))
  (cp/escreen-show-active-screens))

(defun cp/escreen-switch-to-screen-with-ivy-completion ()
  (interactive)
  (let ((collection (cp/escreen-ivy-collection)))
    (if (> (length collection) 0)
        (let* ((current (car (cp/escreen-ivy-screen-number-to-datum nil (escreen-get-current-screen-number))))
               (prompt (format "switch to escreen [%s]:" current)))
          (ivy-read prompt collection
                    :require-match t
                    :action #'cp/escreen-ivy-action))
      (cp/escreen-show-active-screens))))

(defun cp/escreen-compress ()
  "Compress all screen numbers to remove gaps"
  (interactive)
  (-each-indexed
      (-sort '< (escreen-configuration-screen-numbers))
    (lambda (idx screen-number)
      (let ((shift (- screen-number idx)))
        (progn
          (message "cp/escreen-compress shifting screen %d left by %d" screen-number shift)
          (cp/escreen-move-screen-left screen-number shift)))))
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-goto-screen (n &optional dont-update-current)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-kill-screen (&optional n)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-create-screen (&optional n)
  (cp/escreen-rename-screen)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-install ()
  (cp/escreen-rename-screen nil nil t))

(use-package escreen
  :defer t
  :commands (escreen-create-screen)
  :load-path "lisp"
  :bind-keymap ("C-\\" . escreen-map)
  :general
  (:keymaps '(override)
   :states '(normal motion emacs)
   :prefix ","
   "e" '(:keymap escreen-map :which-key "escreen"))
  (:keymaps '(override)
   :states '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a e" '(:keymap escreen-map :which-key "escreen"))
  (:keymaps '(escreen-map)
   "C-b" nil
   "n"   nil
   "t"   #'escreen-goto-last-screen
   "e"   #'cp/escreen-show-active-screens
   "v"   #'cp/escreen-show-active-screens-vertical
   "f"   #'cp/escreen-show-active-screens-horizontal
   "V"   #'cp/escreen-set-show-active-screens-fun-vertical
   "F"   #'cp/escreen-set-show-active-screens-fun-horizontal
   "A"   #'cp/escreen-set-show-active-screens-fun-auto
   "r"   #'cp/escreen-rename-screen
   "s"   #'cp/escreen-switch-to-screen-with-ivy-completion
   "C"   #'cp/escreen-compress
   "k"   #'escreen-kill-screen
   "H"   #'cp/escreen-move-screen-left
   "L"   #'cp/escreen-move-screen-right
   "h"   #'escreen-goto-prev-screen
   "l"   #'escreen-goto-next-screen)
  :config
  (progn
    (setq escreen-max-screens 30)
    (advice-add 'escreen-goto-screen   :after #'cp/advice/escreen-goto-screen)
    (advice-add 'escreen-kill-screen   :after #'cp/advice/escreen-kill-screen)
    (advice-add 'escreen-create-screen :after #'cp/advice/escreen-create-screen)
    (advice-add 'escreen-install       :after #'cp/advice/escreen-install)
    (escreen-install)))



;; org
; http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
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

(defun cp/org-echo-link-at-point ()
  (let ((raw-link (org-element-property :raw-link (org-element-context))))
    (message "%s" raw-link)))

(defun cp/org-echo-link-matching (regex)
    (let ((raw-link (org-element-property :raw-link (org-element-context))))
      (when (s-matches? regex raw-link)
        (cp/org-echo-link-at-point))))

(defun cp/org-link-auto-desc-from-abbrev-tags (link desc)
  (let ((abbrevs
         (append (mapcar 'car org-link-abbrev-alist-local)
                 (mapcar 'car org-link-abbrev-alist))))
    (catch 'found
      (dolist (abbrev abbrevs)
        (let ((s (format "^%s:\\(.+\\)" abbrev)))
          (when (string-match s link)
            (throw 'found (match-string 1 link)))))
      desc)))

(defun cp/org-surround (c)
  (let ((spc (looking-back " " nil)))
    (progn
      (evil-backward-WORD-begin)
      (insert-char c)
      (evil-forward-WORD-end)
      (forward-char)
      (insert-char c))
    (when spc (insert-char ?\s))))

(defun cp/org-surround-tilda ()
  (interactive)
  (cp/org-surround ?~))

(defun cp/org-surround-equal ()
  (interactive)
  (cp/org-surround ?=))

(defun cp/org-surround-star ()
  (interactive)
  (cp/org-surround ?*))

(setq cp/org-username-command-alist
      '((darwin     . "dscacheutil -q user | awk -F: '$1 ~ /name/ {print $2}'")
        (gnu/linux  . "getent passwd | cut -d: -f1")))

(defun cp/org-username-list-all ()
  (let* ((cmd (alist-get system-type cp/org-username-command-alist)))
    (when cmd
      (split-string (shell-command-to-string cmd)))))

(cp/make-symbol-caching-version-of #'cp/org-username-list-all-caching #'cp/org-username-list-all 86400)

(defun cp/org-ivy-usernames (username)
  (ivy-read "username: "
            (cp/org-username-list-all-caching)
            :require-match t
            :initial-input username))

(defun cp/org-complete-user-name-at-point ()
  (interactive)
  (let* ((partial  (word-at-point))
         (username (cp/org-ivy-usernames partial)))
    (when username
      (progn
        (when partial
          (backward-kill-word 1))
        (insert username)))))

(defun cp/advice/org-next-link (&optional search-backward)
  (cp/org-echo-link-at-point))

(defun cp/advice/org-previous-link ()
  (cp/org-echo-link-at-point))

(defun cp/org-sort-org-todo-keywords-to-alist ()
  "Take `org-todo-keywords' and turn it into an association list.
The key is the todo keyword and the value is its relative position in the list."
  ;; CR-soon cperl: This isn't quite right as org-todo-keywords is
  ;; actually a list of sequences and "|" is optional, if it is
  ;; omitted, then the last keyword is the finished state
  (let* ((todo-keywords-split-by-finished
          (->>
           org-todo-keywords
           (nth 0)
           (-filter
            (lambda (elem)
              (not
               (or
                (equal elem 'sequence)
                (equal elem 'type)))))
           (-split-on "|")))
         (not-finished (nth 0 todo-keywords-split-by-finished))
         (finished (nth 1 todo-keywords-split-by-finished))
         (n-finished-states (length finished))
         (rotated (-rotate n-finished-states (-concat not-finished finished))))
    (-map-indexed
     (lambda (idx elem)
       (let ((todo (replace-regexp-in-string "(.*)$" "" elem)))
         `(,todo . ,idx)))
     rotated)))

(defun cp/org-sort-org-todo-keywords-max ()
  "Determine the largest index in org-todo-keywords"
  (->>
   (cp/org-sort-org-todo-keywords-to-alist)
   (-map    #'cdr)
   (-reduce #'max)))

(defun cp/org-sort-todo-to-int ()
  (-some->
   (org-entry-get (point) "TODO")
   (assoc (cp/org-sort-org-todo-keywords-to-alist))
   (cdr)))

(defun cp/org-sort-item-to-int ()
  (let* ((item (org-entry-get (point) "ITEM"))
         (item (replace-regexp-in-string "^\\** *" "" item)))
    ;; Make headings with "Notes" sort before everything else
    (if (string-match "\\bNotes\\b" item)
        -1
      (1+ (cp/org-sort-org-todo-keywords-max)))))

(defun cp/org-sort-key ()
  (let* ((todo (cp/org-sort-todo-to-int))
         (todo-int (or todo (cp/org-sort-item-to-int)))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (string-to-char priority)))
    (format "%03d %03d" todo-int priority-int)))

(defun cp/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'cp/org-sort-key)
  ;; there may be a better way to do this, but for now its refolds things the way I want after sorting
  (funcall (general-simulate-key "TAB TAB")))

(defmacro cp/generate-category-agenda-cmds (letter desc categories include days-out &optional options)
  "Generate a set of commands for org-agenda-custom-commands.

Generate a form that looks like (key desc (cmd1 cmd2 ...)
general-settings-for-whole-set) where cmd1 is an agenda command, cmd2
though cmd5 are tags-todo searches and all of them are restricted to
just the categories that we've been passed.  Whether or not we're
including those categories, or excluding those categories is
controlled by `include'."
  (let* ((options (or options ()))
         (include-exclude (if include "+" "-"))
         (category-regex (s-join "\|" categories))
         (todo-keywords '("NEXT" "WAIT" "DPND" "DFER"))
         (scheduled-or-deadline-days-out (1- days-out))
         (search-string-fmt
          (apply-partially
           #'format
           (s-concat
            (s-join
             "|"
             '("-DEADLINE={.+}&-SCHEDULED={.+}&%sCATEGORY={%s}"
               "+DEADLINE>=\"<+%dd>\"&%sCATEGORY={%s}"
               "+SCHEDULED>=\"<+%dd>\"&%sCATEGORY={%s}"))
            "/%s")
           include-exclude category-regex
           scheduled-or-deadline-days-out include-exclude category-regex
           scheduled-or-deadline-days-out include-exclude category-regex))
         (tags-todo-cmds
          (-map
           (lambda (todo)
             `(tags-todo
              ,(funcall search-string-fmt todo)
              ((org-agenda-overriding-header
                ,(format
                  "%s No deadline, not scheduled, or deadline/scheduled is %d days out or more"
                  todo
                  days-out))
               (org-agenda-sorting-strategy '(deadline-up tsia-up)))))
           todo-keywords))
         (category-filter
          (-map
           (lambda (category) (s-concat include-exclude category))
           categories))
         (agenda-header-fmt
          (format "Agenda for the next %dd (W%%W) (generated %%Y-%%m-%%d %%H:%%M:%%S)" days-out))
         (agenda-header `(format-time-string ,agenda-header-fmt))
         (forms
          `(,letter
            ,desc
            ,(cons
              `(agenda ""
                       ((org-agenda-span ,days-out)
                        (org-deadline-warning-days ,days-out)
                        (org-agenda-overriding-header ,agenda-header)
                        (org-agenda-sorting-strategy '(time-up deadline-up tsia-up))))
              tags-todo-cmds)
            ; CR cperl: It would be nice to use org-agenda-category-filter-preset here,
            ; but it has issues when using sticky agenda and generating multiple agendas
            ; and the filter seems to be global and pressing "r" doesn't set it back to
            ; this value
            ((org-agenda-category-filter (quote ,category-filter))
             ,@options))))
    `(quote ,forms)))

(defun cp/org-agenda-tagged-by-person (search-string)
  ;; CR-someday cperl: Have to figure out how to make C-u r in the buffer work correctly.
  ;; Right now it regenerated the buffer with a different tag, but winds up leaving it
  ;; named incorrectly.  I was thinking one way around it might be to substitute in the
  ;; proper to call to this function for `org-agenda-redo-command' and the `redo-cmd' text
  ;; property, but I could quite get it working.
  (let ((selected
         (or search-string
             (completing-read
              "Agenda for: "
              (-keep
               (lambda (tag)
                 (let ((tag (substring-no-properties (car tag))))
                   (if (s-matches? "^@" tag) tag)))
               (org-global-tags-completion-table))
              #'identity
              t))))
    (org-tags-view '(4) selected)))

(defun cp/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun cp/org-appt-disp-window (min-to-app new-time msg)
  ;; Add code here to encode the logic of displaying the appt notification.
  ;; You have to test whether `min-to-app' is a list or an atom, which can be done with `atom'
  ;; You'll need to think about how you want to override behavior for your various use cases
  ;;
  ;; If min-to-app is a list, then it means there are multiple notifications and you should handle them all.
  ;;
  ;; Example:
  ;; ("5" "5") "Thu Jan 17 " (#("06:55 Foo" 6 9 (org-heading t org-category "foo" face org-level-1 fontified t)) #("06:55 Bar" 6 9 (org-heading t org-category "foo" face org-level-1 fontified t)))
  (message "%S %S %S" min-to-app new-time msg)
  t)

;; Taken from https://emacs.stackexchange.com/questions/28782/avoiding-underlined-spaces-between-two-lines
;; and modified slightly
(defface cp/org-dont-underline-leading-spaces-in-multiline-links '((t :underline nil))
  "Avoid underlining of indentation.")

(defun cp/org-search-underlined-leading-spaces-in-multiline-links (limit)
  "Match function for `cp/org-dont-underline-leading-spaces-in-multiline-links'."
  (let (ret face)
    (while
        (and
         (setq ret (re-search-forward "^[[:space:]]+" limit t))
         (or (null (setq face (plist-get (text-properties-at (match-beginning 0)) 'face)))
             (not (eq face 'org-link)))))
    ret))

(defun cp/org-dont-underline-leading-spaces-in-multiline-links ()
  "Remove underlining at indents."
  (font-lock-add-keywords
   'org-mode
   '((cp/org-search-underlined-leading-spaces-in-multiline-links 0 'cp/org-dont-underline-leading-spaces-in-multiline-links t))
   'append))

(use-package org
  :defer t
  :general
  (:keymaps '(org-mode-map org-agenda-mode-map)
   :states  '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
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
   "o L"     #'org-shiftmetaright)
  (:keymaps '(org-mode-map)
   :states  '(normal motion)
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
   :states  '(insert)
   "M-RET"   #'org-meta-return
   "M-."     #'cp/org-surround-tilda
   "M-,"     #'cp/org-surround-equal
   "M-u"     #'cp/org-complete-user-name-at-point)
  (:keymaps '(org-agenda-mode-map)
   :states  '(emacs)
   "j"       #'org-agenda-next-line
   "k"       #'org-agenda-previous-line
   "h"       #'left-char
   "l"       #'right-char
   "G"       #'evil-goto-line
   "gg"      #'evil-goto-first-line
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
  :config
  (progn
    (use-package appt
      :config
      (progn
        (appt-activate t)
        (setq appt-message-warning-time 15)
        ; (setq appt-display-mode-line nil)
        ; (setq appt-display-interval appt-message-warning-time)
        ;; Add code here to automatically run `cp/org-agenda-to-appt'
        ;; at certain times, you need to run this to move any
        ;; scheduled or deadline tasks from org to appt for
        ;; notifications.
        (setq appt-disp-window-function #'cp/org-appt-disp-window)
        (setq appt-delete-window-function (lambda () t))))
    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))
    (use-package org-habit
      :config
      (progn
        (setq org-habit-today-glyph ?t)
        (setq org-habit-completed-glyph ?d)
        (setq org-habit-graph-column 65)))
    (use-package ol-man)
    (use-package org-tempo)
    (use-package ob-async
      ;; 2024-02-01 cperl: Disabled for now after some issues running
      ;; non-async src blocks, something about wrong number of
      ;; arguments or something like that.
      :disabled t
      :config
      ;; 2022-10-22 cperl: A workaround for :async not working
      ;; sometimes as described at
      ;; https://github.com/astahlman/ob-async/issues/75
      (defun no-hide-overlays (orig-fun &rest args)
        (setq org-babel-hide-result-overlays nil))
      (advice-add 'ob-async-org-babel-execute-src-block :before #'no-hide-overlays))
    (setq org-adapt-indentation t)
    (setq org-startup-folded t)
    (setq org-tags-column -90)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-todo-keywords
          '((sequence "NEXT(n)" "WAIT(w)" "DFER(r)" "DPND(x)" "|" "DONE(d!)" "CNCL(c!)")))
    (setq org-todo-keyword-faces
          '(("DFER" . "#767676")
            ("DPND" . "#767676")
            ("WAIT" . "#8C5353")
            ("CNCL" . "#FFFFFF")
            ("DONE" . "#FFFFFF")))
    (setq org-agenda-files '("~/org"))
    (setq org-agenda-tags-column -90)
    (setq org-agenda-custom-commands
          `(,(cp/generate-category-agenda-cmds "c" "Captured" ("capture") t 7)
            ,(cp/generate-category-agenda-cmds "h" "House"    ("house")   t 7)
            ,(cp/generate-category-agenda-cmds "g" "General"  ("general") t 7)
            ,(cp/generate-category-agenda-cmds "e" "Everything else" ("capture" "house" "general") nil 7)
            ("@" "Tagged By Person" cp/org-agenda-tagged-by-person nil
             ((org-agenda-sorting-strategy '(tag-up todo-state-up ts-up tsia-up))))))
    (setq org-agenda-sorting-strategy '(todo-state-up deadline-up tsia-up))
    ;; 2024-06-01 cperl: Remove the `string-join' if/when you move to using
    ;; `org-indent-mode' (which turns off `org-adapt-indentation')
    (setq org-capture-templates
          `(("n" "Next Action" entry (file "~/org/capture.org")
             ,(string-join '("* NEXT %?" ":PROPERTIES:" ":CAPTURED: %U" ":END:") "\n  ")
             :empty-lines 1)))
    (setq org-link-abbrev-alist
          '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-hide-block-startup t)
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-block-separator ?-)
    (setq org-agenda-scheduled-leaders '("   Scheduled:" "  Sched %3dx:"))
    (setq org-agenda-deadline-leaders '("Deadline due:" "     In %3dd:" "   %4dd ago:"))
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-format-date "%a %Y-%m-%d")
    (setq org-agenda-sticky t)
    (setq org-attach-use-inheritance t)
    (setq org-log-into-drawer t)
    (setq org-refile-use-cache t)
    (setq org-catch-invisible-edits 'error)
    (setq org-ctrl-k-protect-subtree t)
    (setq org-cycle-include-plain-lists 'integrate)
    (setq org-hide-leading-stars t)
    (setq org-link-make-description-function  #'cp/org-link-auto-desc-from-abbrev-tags)
    (run-with-idle-timer 30 t
                         (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))
    (advice-add  'org-next-link     :after #'cp/advice/org-next-link)
    (advice-add  'org-previous-link :after #'cp/advice/org-previous-link)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell  . true)
       (python . true)
       (awk    . true)
       (sed    . true)
       (R      . true)
       (calc   . true)))
    (cp/org-dont-underline-leading-spaces-in-multiline-links)
    (add-hook
     'org-mode-hook
     (lambda ()
       (progn
         (auto-fill-mode)
         (setq fill-column 90)
         (setq indent-tabs-mode nil)
         (define-and-bind-text-object "~" "\\~" "\\~")
         (define-and-bind-text-object "*" "\\*" "\\*")
         (define-and-bind-text-object "=" "\\=" "\\=")
         (add-hook 'write-contents-functions
                   (lambda () (save-excursion (delete-trailing-whitespace)))))))
    (add-hook 'org-src-mode-hook    (lambda () (setq electric-indent-mode nil)))
    (remove-hook 'org-mode-hook 'org-eldoc-load)))



;; projectile
(defvar cp/projectile-project-full-feature-name t)

(defun cp/projectile-project-name (project-root)
  (if (string-match "+share+" project-root)
      (let ((feature-base
             (expand-file-name (locate-dominating-file project-root "+share+"))))
        (if cp/projectile-project-full-feature-name
            (let ((prefix
                   (thread-last (locate-dominating-file feature-base "+clone+")
                     (expand-file-name)
                     (file-name-directory)
                     (directory-file-name)
                     (file-name-directory)))
                  (d (directory-file-name feature-base)))
              (replace-regexp-in-string (format "^%s" prefix) "" d))
          (thread-last (file-name-directory feature-base)
            (directory-file-name)
            (file-name-nondirectory))))
    (projectile-default-project-name project-root)))

(setq cp/projectile-projects-cache-by-time (make-hash-table :test 'equal))

(defun cp/projectile-projects-cache-by-time-sync (data)
  (let* ((projectile-keys (projectile-hash-keys data))
         (time-keys       (projectile-hash-keys cp/projectile-projects-cache-by-time))
         (keys-to-add     (-difference projectile-keys time-keys))
         (keys-to-delete  (-difference time-keys projectile-keys)))
    (progn
      (dolist (project keys-to-add)
        (puthash project (current-time) cp/projectile-projects-cache-by-time)
        (projectile-add-known-project project))
      (dolist (project keys-to-delete)
        (remhash project cp/projectile-projects-cache-by-time)
        (projectile-remove-known-project project)))))

(defun cp/advice/projectile-serialize (orig-fun data filename)
  (cond ((eq filename projectile-cache-file) (cp/projectile-projects-cache-by-time-sync data))
        ((eq filename projectile-known-projects-file) nil)
        (t (apply orig-fun data filename ()))))

(defun cp/advice/projectile-unserialize (orig-fun filename)
  (cond ((eq filename projectile-cache-file) nil)
        ((eq filename projectile-known-projects-file)
         (-map
          #'abbreviate-file-name
          (projectile-hash-keys cp/projectile-projects-cache-by-time)))
        (t (apply orig-fun filename ()))))

(defun cp/advice/projectile-maybe-invalidate-cache (orig-fun force)
  (or
   (when (and (not force) (projectile-project-p))
     (let* ((vcs (projectile-project-vcs))
            (cache-invalidate-proxy
             (cond ((eq vcs 'hg)  ".hg/dirstate")
                   ((eq vcs 'git) ".git/logs/HEAD"))))
       (when cache-invalidate-proxy
         (let* ((project-root (projectile-project-root))
                (project-cached-at-or-before (gethash project-root cp/projectile-projects-cache-by-time))
                (proxy (concat project-root cache-invalidate-proxy)))
           (when (and project-cached-at-or-before (file-exists-p proxy))
             (let ((proxy-time-stamp (float-time (nth 5 (file-attributes proxy))))
                   (project-time-stamp (float-time project-cached-at-or-before)))
               (when (> proxy-time-stamp project-time-stamp)
                 (progn
                   (message
                    "Project %s was cached at or before %s, which is older than mtime of %s, invalidating cache"
                    project-root
                    (current-time-string project-cached-at-or-before)
                    cache-invalidate-proxy)
                   (projectile-invalidate-cache nil)))))))))
   (funcall orig-fun force)))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :commands (projectile-project-p projectile-project-name projectile-project-root)
  :general
  (:keymaps '(override)
   :states  '(normal motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a p"   '(:keymap projectile-command-map :which-key "projectile"))
  (:states '(normal motion insert emacs)
   "C-c p" '(:keymap projectile-command-map :which-key "projectile"))
  :config
  (progn
    (setq projectile-enable-caching t
          projectile-cache-file
          (concat temporary-file-directory "projectile.cache")
          projectile-known-projects-file
          (concat temporary-file-directory "projectile-bookmarks.eld" )
          projectile-completion-system 'ivy
          projectile-project-name-function #'cp/projectile-project-name
          projectile-switch-project-action #'projectile-dired)
    (add-to-list 'projectile-project-root-files-bottom-up "cscope.files")
    (projectile-mode)))



;; counsel-projectile
(use-package counsel-projectile
  :defer t
  :after (projectile)
  )



; ios-config-mode
(use-package ios-config-mode
  :defer t
  :commands ios-config-mode
  :config
  (zenburn-with-color-variables
    (setq ios-config-command-face  `((t . (:foreground ,zenburn-cyan))))
    (setq ios-config-ipadd-face    `((t . (:foreground ,zenburn-red))))
    (setq ios-config-toplevel-face `((t . (:foreground ,zenburn-yellow))))))


(use-package embark
  :defer t
  :commands (embark-act)
  :general
  (:keymaps '(override)
   "C-c ." #'embark-act))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0" "4555c851795f0e0fd572ba82208373b0c32aaffa78289e983d4b25cd1557f472" "a1e99cb36d6235abbe426a0a96fc26c006306f6b9d2a64c2435363350a987b4c" default))
 '(package-selected-packages '(rainbow-mode let-alist)))

; zenburn theme overrides
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(info-node                   ((t (:foreground ,zenburn-red-3))))
   `(diff-added                  ((t (:foreground ,zenburn-green :weight bold))))
   `(diff-removed                ((t (:foreground ,zenburn-red))))
   `(linum                       ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg))))
   '(dired-perm-write            ((t nil)))
   '(hl-line                     ((t (:background "#4F4F4F"))))
   '(ivy-cursor                  ((t (:foreground "#000000" :background "#d6d6d6"))))
   `(ivy-current-match           ((t (:foreground nil :background nil :underline nil))))
   '(ivy-minibuffer-match-face-1 ((t (:foreground nil :background nil :underline nil))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-red-2    :background nil))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-blue-1  :background nil))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-yellow-2 :background nil))))
   `(ivy-virtual                 ((t (:inherit font-lock-type-face))))
   '(swiper-line-face            ((t (:background "#4F4F4F"))))
   '(swiper-match-face-1         ((t (:foreground nil :background nil :underline nil))))
   `(swiper-match-face-2         ((t (:foreground "white" :weight bold :background ,zenburn-red-2))))
   `(swiper-match-face-3         ((t (:foreground "white" :weight bold :background ,zenburn-blue-1))))
   `(swiper-match-face-4         ((t (:foreground "white" :weight bold :background ,zenburn-yellow-2))))
   `(org-agenda-done             ((t (:foreground ,zenburn-green+4 :background nil))))))

;; I don't quite understand why this is necessary, somehow related to:
;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27
(enable-theme 'zenburn)


; 2014-04-08: local emacs overrides
(let ((local "~/.emacs.local"))
  (when (file-exists-p local) (load-file local)))

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
