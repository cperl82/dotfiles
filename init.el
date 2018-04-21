;; Required as of emacs 25
(package-initialize)

;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(setq cp/el-get-user-recipe-dir (concat user-emacs-directory "user-recipes"))
(add-to-list 'el-get-recipe-path cp/el-get-user-recipe-dir)
(el-get
 'sync
 '(avy
   ace-window
   color-theme-zenburn
   company-mode
   counsel-projectile
   dash
   diminish
   elisp-slime-nav
   emacs-request
   evil
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
   markdown-mode
   nasm-mode
   org-mode
   origami
   projectile
   s
   smartparens
   smex
   swiper
   systemd-mode
   systemtap-mode
   tuareg-mode
   undo-tree
   use-package
   vagrant-tramp
   wgrep
   which-key
   windsize
   xcscope
   yaml-mode))


;; Base packages
(require 'general)
(require 'use-package)
(require 'diminish)
(require 'dash)
(require 's)
(require 'f)
(general-override-mode)


;; el-get helpers: these depend on packages that el-get installs,
;; therefore they can't be used to help bootstrap el-get in any way
(defun cp/el-get-list-recipes-without-hash ()
  "Return a list of installed recipes that do not have :checkout in their recipe"
  (thread-last (el-get-package-status-recipes)
    ;; filter out el-get, we don't want to version lock it
    (seq-filter (lambda (r) (not (equal (plist-get r :name) 'el-get))))
    ;; filter out recipes that are builtin to this version of emacs
    (seq-filter (lambda (r)
                  (if-let (builtin (plist-get r :builtin))
                      (not (version<= builtin emacs-version))
                    t)))
    (seq-filter (lambda (r)
                  (not
                   (or
                    (plist-member r :checkout)
                    (plist-member r :checksum)))))))

(defun cp/el-get-list-package-names-without-hash ()
  "Return a list of el-get package names that do not have :checkout in their property list"
  (thread-last (cp/el-get-list-recipes-without-hash)
    (seq-map (lambda (r) (plist-get r :name)))))

(defun cp/el-get-annotate-package-names (package-names)
  "Return (PACKAGE-NAME checksum filename) for each PACKAGE-NAME"
  (seq-map
   (lambda (package-name)
     (let* ((type (el-get-package-type package-name))
            (compute-checksum (el-get-method type :compute-checksum))
            (checksum (and compute-checksum (funcall compute-checksum package-name))))
       `(,package-name ,checksum ,(el-get-recipe-filename package-name))))
   package-names))



;; 2014-04-26: Loading other stuff
(add-to-list 'load-path (concat user-emacs-directory "lisp"))



;; Misc
(setq ad-redefinition-action  'accept
      c-default-style         "linux"
      column-number-mode      t
      confirm-kill-emacs      'y-or-n-p
      gc-cons-threshold       100000000
      inhibit-startup-message t
      make-backup-files       nil
      split-height-threshold  nil
      split-width-threshold   60)

;; 2015-09-11 Enable narrowing command which are disabled by default
(put 'narrow-to-region 'disabled nil)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun cp/format-default-dir-for-mode-line (d max-length)
  (let* ((reduced
          (if (string-match (format "^%s" (getenv "HOME")) d) (replace-match "~" t t d) d))
         (reduced (replace-regexp-in-string "/$" "" reduced)))
    (if (> (length reduced) max-length)
        (let* ((n 0)
               (further-reduced nil)
               (pieces
                (reverse (remove-if (lambda (s) (equal "" s)) (split-string reduced "/"))))
               (len (length pieces)))
          (catch 'done
            (while (< n len)
              (setq further-reduced
                    (if further-reduced
                        (format "%s/%s" (nth n pieces) further-reduced)
                      (nth n pieces)))
              (when (> (length further-reduced) max-length) (throw 'done further-reduced))
              (setq n (1+ n)))))
      reduced)))

(defmacro cp/make-symbol-caching-version-of (name f timeout)
  `(progn
     (fset
      ,name
      (lexical-let*
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

;; 2014-04-22 mode-line-format
(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                (:eval (cp/format-default-dir-for-mode-line default-directory 40))
                " "
                mode-line-position
                evil-mode-line-tag
                (vc-mode vc-mode)
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; 2014-05-07 cperl: function to revert all buffers
(defun revert-buffer-all ()
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

;; 2017-01-12 cperl: function to help split windows the way I like it
(defun cp/split-windows-sensibly (&rest r)
  (when (eq (length (window-list)) 1)
    (let* ((columns (window-size nil t))
           (new (1+ (/ columns 2))))
      (progn
        (message "Setting split-width-threshold to %d" new)
        (setq split-width-threshold new)))))

(advice-add 'split-window-sensibly :before #'cp/split-windows-sensibly)
(advice-add 'split-window-horizontally :before #'cp/split-windows-sensibly)

;; 2017-08-17 cperl: misc stuff
(defun cp/c-mode-hook-setup ()
  (hs-minor-mode))
(add-hook 'c-mode-hook #'cp/c-mode-hook-setup)

(setq cp/normal-prefix "SPC")
(setq cp/non-normal-prefix "M-SPC")

(global-unset-key (kbd "C-h"))
(setq help-char nil)

;; Unbind existing keybindings in evil-motion-state-map
(general-define-key
 :keymaps '(motion)
  "SPC" nil
  ","   nil)

;; Global keybinding that go into evil-motion-state-map and evil-emacs-state-map
(general-define-key
 :keymaps '(motion emacs)
 :prefix ","
  "h" #'cp/evil-highlight-symbol
  "s" #'split-window-vertically
  "v" #'split-window-horizontally
  "x" #'delete-window
  "o" #'delete-other-windows
  "f" #'find-file
  "j" #'dired-jump
  "r" #'find-file-read-only
  "k" #'kill-buffer)

;; Global keybinding that go into evil-motion-state-map, evil-insert-state-map and evil-emacs-state-map
(general-define-key
 :keymaps '(motion insert emacs)
 :prefix cp/normal-prefix
 :non-normal-prefix cp/non-normal-prefix
  "a" '(:ignore t :which-key "applications")
  "b" '(:ignore t :which-key "buffers")
  "w" '(:ignore t :which-key "windows")
  "f" '(:ignore t :which-key "files")
  "f f" #'find-file
  "f r" #'find-file-read-only
  "f j" #'dired-jump
  "b b" #'switch-to-buffer
  "b k" #'kill-buffer
  "b K" #'kill-buffer-and-window
  "b r" #'revert-buffer
  "b R" #'revert-buffer-all
  "b f" #'(lambda () (interactive) (message (buffer-file-name)))
  "w s" #'split-window-vertically
  "w v" #'split-window-horizontally
  "w K" #'kill-buffer-and-window
  "w o" #'delete-other-windows
  "w x" #'delete-window
  "w =" #'balance-windows)

(general-define-key
 :keymaps '(motion insert emacs)
  "M-o" #'other-window)


;; which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (progn
    (which-key-mode)))



;; evil
;; 2014-03-28: Functions to support selecting something in Visual mode
;; and then automatically start searching for it by pressing "/" or "?"
(defun cp/evil-highlight-symbol ()
  "Do everything that `*' would do, but don't actually jump to the next match"
  (interactive)
  (let* ((string (evil-find-symbol t))
         (case-fold-search
          (unless (and search-upper-case
                       (not (isearch-no-upper-case-p string nil)))
            case-fold-search)))
    (setq isearch-regexp t)
    (setq isearch-forward t)
    (setq string (format "\\_<%s\\_>" (regexp-quote string)))
    (setq isearch-string string)
    (isearch-update-ring string t)
    (setq string (evil-search-message string t))
    (evil-flash-search-pattern string t)))

(evil-define-operator cp/evil-search (beg end forward)
  (let* ((search-string (buffer-substring-no-properties beg end))
         (quoted-string (regexp-quote search-string)))
    (setq isearch-forward forward)
    (evil-search quoted-string forward t)))

(evil-define-operator cp/evil-search-forward (beg end type)
  (cp/evil-search beg end t))

(evil-define-operator cp/evil-search-backward (beg end type)
  (cp/evil-search beg end nil))

(use-package evil
  :diminish undo-tree-mode
  :general
  (:keymaps 'override
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "C-l" #'evil-window-right)
  (:keymaps '(motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w h" #'evil-window-left
   "w j" #'evil-window-down
   "w k" #'evil-window-up
   "w l" #'evil-window-right)
  (:keymaps '(visual)
   "/"   #'cp/evil-search-forward
   "?"   #'cp/evil-search-backward
   "TAB" #'indent-region)
  :init
  (progn
    (setq-default evil-symbol-word-search t)
    (setq-default evil-flash-delay 5)
    (setq-default evil-move-beyond-eol t)
    (setq-default evil-want-C-i-jump nil))
  :config
  (progn
    (evil-mode 1)
    (evil-select-search-module 'evil-search-module 'isearch)))



;; evil-surround
(use-package evil-surround
  :config
  (progn
    (global-evil-surround-mode 1)))



;; buffer-move
(use-package buffer-move
  :defer t
  :commands (buf-move-down buf-move-up buf-move-left buf-move-right)
  :init
  :general
  (:states '(motion insert emacs)
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
  (:keymaps '(motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a a"   '(:ignore t :which-key "avy")
   "a a c" #'avy-goto-char
   "a a C" #'avy-goto-char-2
   "a a W" #'avy-goto-word-1)
  :config
  (progn
    (setq avy-background t)))

;; ace-window
(use-package ace-window
  :defer t
  :general
  (:keymaps '(normal insert motion emacs)
   "M-O" #'ace-window)
  (:keymaps '(motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w a" #'ace-window))



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
  :config
  (progn
    (setq uniquify-buffer-name-style 'forward)
    (setq uniquify-strip-common-suffix nil)))


;; swiper / ivy / counsel / smex
(use-package smex
  :defer t)

(use-package ivy
  :defer t
  :diminish ivy-mode
  :general
  (:keymaps '(ivy-minibuffer-map)
   "<up>"   #'ivy-previous-history-element
   "<down>" #'ivy-next-history-element)
  ("C-s"    #'counsel-grep-or-swiper)
  :init
  (progn
    (use-package ivy-rich
      :config
      (progn
        (setq ivy-rich-path-style 'relative)
        (setq ivy-rich-switch-buffer-name-max-length 40)
        (setq ivy-rich-switch-buffer-project-max-length 25)
        (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)))
    (setq ivy-height 10)
    (setq ivy-initial-inputs-alist nil)
    ;; 2017-05-06: `ivy--regex-ignore-order' doesn't seem to work well with swiper as the
    ;; buffer highlights don't seem to be applied correctly.  However, its useful when
    ;; using `counsel-M-x', so enable it for that (e.g. typing `dired find' to find
    ;; `find-dired').
    (setq ivy-re-builders-alist
          '((counsel-M-x . ivy--regex-ignore-order)
            (t . ivy--regex-plus)))
    (setq ivy-format-function 'ivy-format-function-arrow)
    (setq ivy-count-format "%d/%d ")
    (ivy-mode 1)))

(use-package counsel
  :defer t
  :diminish counsel-mode
  :init
  (progn
    ;; 2018-04-14 cperl: copying https://oremacs.com/2018/03/05/grep-exclude
    (setq counsel-git-cmd "rg --files")
    (defalias #'cp/project-files #'counsel-git)
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s .")
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

(use-package dired
  :defer t
  :general
  (:keymaps '(dired-mode-map)
   :states  '(normal)
   "h"   #'dired-up-directory
   "j"   #'dired-next-line
   "k"   #'dired-previous-line
   "l"   #'dired-find-file
   "n"   #'evil-search-next
   "N"   #'evil-search-previous
   "?"   #'evil-search-backward
   "G"   #'evil-goto-line
   "gg"  #'evil-goto-first-line
   "M-q" #'dired-kill-subdir
   "M-n" #'dired-next-subdir
   "M-p" #'dired-prev-subdir
   "q"   #'bury-buffer
   "TAB" #'cp/dired-tab-dwim
   "o"   #'dired-find-file-other-window
   "r"   #'revert-buffer
   "."   #'cp/dired-toggle-hiding-dotfiles
   "SPC" nil)
  :config
  (progn
    (evil-set-initial-state 'dired-mode 'normal)
    (use-package dired-x)
    (put 'dired-find-alternate-file 'disabled nil)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))



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
  (:keymaps '(normal visual motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a c" '(:keymap cscope-command-map :which-key "cscope"))
  (:keymaps '(cscope-list-entry-keymap)
   :states  '(motion)
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



;; 2016-09-10 cperl: better alignment of property lists
; http://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
(defun cp/lisp-indent-function (indent-point state)
       "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
       (let ((normal-indent (current-column))
             (orig-point (point)))
         (goto-char (1+ (elt state 1)))
         (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
         (cond
          ;; car of form doesn't seem to be a symbol, or is a keyword
          ((and (elt state 2)
                (or (not (looking-at "\\sw\\|\\s_"))
                    (looking-at ":")))
           (if (not (> (save-excursion (forward-line 1) (point))
                       calculate-lisp-indent-last-sexp))
               (progn (goto-char calculate-lisp-indent-last-sexp)
                      (beginning-of-line)
                      (parse-partial-sexp (point)
                                          calculate-lisp-indent-last-sexp 0 t)))
           ;; Indent under the list or under the first sexp on the same
           ;; line as calculate-lisp-indent-last-sexp.  Note that first
           ;; thing on that line has to be complete sexp since we are
           ;; inside the innermost containing sexp.
           (backward-prefix-chars)
           (current-column))
          ((and (save-excursion
                  (goto-char indent-point)
                  (skip-syntax-forward " ")
                  (not (looking-at ":")))
                (save-excursion
                  (goto-char orig-point)
                  (looking-at ":")))
           (save-excursion
             (goto-char (+ 2 (elt state 1)))
             (current-column)))
          (t
           (let ((function (buffer-substring (point)
                                             (progn (forward-sexp 1) (point))))
                 method)
             (setq method (or (function-get (intern-soft function)
                                            'lisp-indent-function)
                              (get (intern-soft function) 'lisp-indent-hook)))
             (cond ((or (eq method 'defun)
                        (and (null method)
                             (> (length function) 3)
                             (string-match "\\`def" function)))
                    (lisp-indent-defform state indent-point))
                   ((integerp method)
                    (lisp-indent-specform method state
                                          indent-point normal-indent))
                   (method
                                      (funcall method indent-point state))))))))

;; lisp-mode
(use-package lisp-mode
  :defer t
  :config
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (setq lisp-indent-function #'cp/lisp-indent-function)
     (setq indent-tabs-mode nil)
     (hs-minor-mode)
     (hs-hide-all))))



;; elisp-slime-nav
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
  :general
  (:keymaps '(elisp-slime-nav-mode-map)
   :states  '(motion)
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
     `(tuareg-mode ,cp/tuareg-mode-hs-start-regexp nil nil  cp/tuareg-mode-hs-forward-sexp-fun))
    (add-hook 'tuareg-mode-hook (lambda () (hs-minor-mode)))))



;; hideshow
(use-package hideshow
  :defer t
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
    (setq sh-basic-offset n)
    (setq sh-indentation n)))

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
   :states  '(motion)
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



;; smartparens/evil-smartparens
; consider stealing some keybindings from https://github.com/expez/evil-smartparens/issues/19
(defun cp/enable-evil-smartparens ()
  (progn
    (smartparens-strict-mode)
    (evil-smartparens-mode)))

(use-package evil-smartparens
  :defer t
  :init
  (progn
    (add-hook 'lisp-mode-hook       #'cp/enable-evil-smartparens)
    (add-hook 'emacs-lisp-mode-hook #'cp/enable-evil-smartparens))
  :config
  (progn
    (sp-local-pair '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "'" nil :actions nil)
    (sp-local-pair '(lisp-interaction-mode lisp-mode emacs-lisp-mode) "`" nil :actions nil)))



;; highlight-parentheses
(use-package highlight-parentheses
  :defer t
  :config
  (progn
    (zenburn-with-color-variables
      (setq hl-paren-colors `(,zenburn-red-4 ,zenburn-green ,zenburn-yellow-2 ,zenburn-blue+1)))))



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
  (:states '(normal insert motion visual emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w r" #'cp/hydra-windsize/body))



;; winner
(use-package winner
  :defer t
  :general
  (:keymaps '(motion visual emacs)
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
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/escreen-move-screen-left (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'left screen-number n))

(defun cp/escreen-move-screen-right (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'right screen-number n))

(defun cp/escreen-rename-screen (&optional name number suppress-message)
  (interactive "sNew screen name: ")
  (let ((screen-data (escreen-configuration-escreen (or number escreen-current-screen-number)))
        (new-name (cond ((equal name "") nil)
                        ((stringp name) name)
                        (t "default"))))
    (setcar (cdr screen-data) new-name)
    (when (not suppress-message)
      (cp/escreen-get-active-screen-names-with-emphasis))))

(defun cp/escreen-propertize-screen-number (number)
  (let ((star (propertize "*" 'face 'font-lock-string-face)))
    (cond ((eq escreen-current-screen-number number) (format "%d%s" number star))
          (t (format "%d-" number)))))

(defun cp/escreen-get-active-screen-names-with-emphasis ()
  (interactive)
  (let ((output ""))
    (dolist (n (escreen-get-active-screen-numbers))
      (let* ((data (escreen-configuration-escreen n))
             (screen-name (nth 1 data))
             )
        (setq output
              (format "%s  %s %s" output (cp/escreen-propertize-screen-number n) screen-name)))
      (message "escreen: active screens: %s" output))))

(defun cp/escreen-get-active-screen-names-with-emphasis-vertical ()
  (interactive)
  (->>
   (cp/escreen-configuration-screen-numbers-and-names)
   (-sort (lambda (s1 s2) (< (car s1) (car s2))))
   (-map
    (lambda (screen)
      (let ((number (car screen))
            (name   (cdr screen)))
        (format "%s %s" (cp/escreen-propertize-screen-number number) name))))
   (s-join "\n")
   (message "%s"))
  nil)

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
                     (fmt (format "%s %%%ds (%%d buffers)" number width))
                     (n-buffers (->> screen-data (nth 3) (--map (nth 0 it)) (--map (nth 1 it)) (length))))
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
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/escreen-switch-to-screen-with-ivy-completion ()
  (interactive)
  (let ((collection (cp/escreen-ivy-collection)))
    (if (> (length collection) 0)
        (let* ((current (car (cp/escreen-ivy-screen-number-to-datum nil (escreen-get-current-screen-number))))
               (prompt (format "switch to escreen [%s]:" current)))
          (ivy-read prompt collection
                    :require-match t
                    :action #'cp/escreen-ivy-action))
      (cp/escreen-get-active-screen-names-with-emphasis))))

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
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/advice/escreen-goto-screen (n &optional dont-update-current)
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/advice/escreen-kill-screen (&optional n)
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/advice/escreen-create-screen (&optional n)
  (cp/escreen-rename-screen)
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/advice/escreen-install ()
  (cp/escreen-rename-screen nil nil t))

(use-package escreen
  :defer t
  :general
  (:states '(motion emacs)
   ", e" '(:keymap escreen-map :which-key "escreen"))
  (:states '(motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a e" '(:keymap escreen-map :which-key "escreen"))
  (:keymaps '(escreen-map)
   "e" #'cp/escreen-get-active-screen-names-with-emphasis
   "v" #'cp/escreen-get-active-screen-names-with-emphasis-vertical
   "r" #'cp/escreen-rename-screen
   "s" #'cp/escreen-switch-to-screen-with-ivy-completion
   "H" #'cp/escreen-move-screen-left
   "L" #'cp/escreen-move-screen-right
   "k" #'escreen-kill-screen
   "l" #'escreen-goto-next-screen
   "h" #'escreen-goto-prev-screen
   "," #'escreen-goto-last-screen)
  :config
  (progn
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
  (let ((spc (looking-back " ")))
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
            (cp/org-username-list-all)
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
  t)

(use-package org
  :defer t
  :general
  (:keymaps '(org-mode-map org-agenda-mode-map)
   :states  '(motion emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "o"       '(:ignore t :which-key "org")
   "o p"     #'org-previous-link
   "o n"     #'org-next-link
   "o a"     #'org-agenda
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
   :states  '(motion)
   "TAB"     #'org-cycle
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
  (:keymaps '(org-mode-map)
   :states  '(insert)
   "M-RET"   #'org-meta-return
   "M-."     #'cp/org-surround-tilda
   "M-v"     #'cp/org-surround-equal
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
    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))
    (use-package org-habit
      :config
      (progn
        (setq org-habit-today-glyph ?t)
        (setq org-habit-completed-glyph ?d)))
    (use-package appt
      :config
      (progn
        (appt-activate t)
        (setq appt-message-warning-time 15)
        (setq appt-display-interval appt-message-warning-time)
        (setq appt-display-mode-line nil)
        ;; Add code here to automatically run `cp/org-agenda-to-appt' at certain times
        (setq appt-disp-window-function #'cp/org-appt-disp-window)
        (setq appt-delete-window-function (lambda () t))))
    (use-package org-depend)
    (use-package org-man)
    (setq org-tags-column -90)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-todo-keywords
          '((sequence "DFER(r)" "DPND(x)" "WAIT(w)" "NEXT(n)" "|" "DONE(d!)" "CNCL(c@)")))
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
    (setq org-capture-templates
          '(("n" "Next Action" entry
             (file "~/org/capture.org") "* NEXT %?\n  captured: %U"
             :empty-lines 1)
            ("N" "Next Action with Gmail Id" entry
             (file "~/org/capture.org") "* NEXT %?\n  captured: %U\n  [[gmail:%^{gmail id}][%\\1]]"
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
    (setq org-log-into-drawer t)
    (setq org-refile-use-cache t)
    (setq org-catch-invisible-edits 'error)
    (setq org-ctrl-k-protect-subtree t)
    (setq org-cycle-include-plain-lists 'integrate)
    (setq org-hide-leading-stars t)
    (setq org-make-link-description-function  #'cp/org-link-auto-desc-from-abbrev-tags)
    (advice-add  'org-next-link     :after #'cp/advice/org-next-link)
    (advice-add  'org-previous-link :after #'cp/advice/org-previous-link)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell  . true)
       (python . true)
       (awk    . true)
       (sed    . true)
       (R      . true)))
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
         (add-hook 'write-contents-functions (lambda () (save-excursion (delete-trailing-whitespace)))))))
    (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))
    (add-hook 'org-src-mode-hook    (lambda () (setq electric-indent-mode nil)))
    (remove-hook 'org-mode-hook 'org-eldoc-load)))



;; projectile
; 2016-04-25: Advice for the low level projectile functions that manage the cache so I can
; track (roughly) when a project was cached and invalidate the cache if I determine there
; is a good reason (e.g. ".hg/dirstate" is newer than the time the projects file were
; cached). In addition, don't serialize to disk, only keep the cache in memory.
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
  :commands (projectile-project-p)
  :defer t
  :general
  (:states '(motion insert emacs)
   :prefix nil
   :non-normal-prefix nil
   "C-c p" '(:keymap projectile-command-map :which-key "projectile"))
  (:states '(motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a p"   '(:keymap projectile-command-map :which-key "projectile"))
  :init
  (progn
    (advice-add 'projectile-serialize              :around #'cp/advice/projectile-serialize)
    (advice-add 'projectile-unserialize            :around #'cp/advice/projectile-unserialize)
    (advice-add 'projectile-maybe-invalidate-cache :around #'cp/advice/projectile-maybe-invalidate-cache)
    (ivy-set-display-transformer 'projectile-completing-read 'ivy-rich-switch-buffer-transformer))
  :config
  (progn
    (setq projectile-enable-caching t)
    (add-to-list 'projectile-project-root-files-bottom-up "cscope.files")
    (setq projectile-completion-system 'ivy)
    (projectile-mode)))



;; counsel-projectile
(use-package counsel-projectile
  :defer t
  :config
  (progn
    (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer #'ivy-rich-switch-buffer-transformer)
    (ivy-set-display-transformer 'counsel-projectile-find-file        #'ivy-rich-switch-buffer-transformer)
    (ivy-set-display-transformer 'counsel-projectile-find-dir         #'ivy-rich-switch-buffer-transformer)
    (ivy-set-display-transformer 'counsel-projectile                  #'ivy-rich-switch-buffer-transformer)))



; linum
; 2014-04-04: Holy moly its effort to get line numbers like vim!
; http://www.emacswiki.org/emacs/LineNumbers#toc6
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun cp/linum-format (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'linum)))

(unless window-system
  (setq linum-format 'cp/linum-format))



; ios-config-mode
(use-package ios-config-mode
  :defer t
  :commands ios-config-mode
  :config
  (zenburn-with-color-variables
    (setq ios-config-command-face  `((t . (:foreground ,zenburn-cyan))))
    (setq ios-config-ipadd-face    `((t . (:foreground ,zenburn-red))))
    (setq ios-config-toplevel-face `((t . (:foreground ,zenburn-yellow))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("4555c851795f0e0fd572ba82208373b0c32aaffa78289e983d4b25cd1557f472" "a1e99cb36d6235abbe426a0a96fc26c006306f6b9d2a64c2435363350a987b4c" default)))
 '(package-selected-packages (quote (rainbow-mode let-alist))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; zenburn theme overrides
(zenburn-with-color-variables
  (custom-theme-set-faces
   `zenburn
   `(isearch                     ((t (:foreground ,zenburn-bg-05 :weight bold :background ,zenburn-orange))))
   `(lazy-highlight              ((t (:foreground ,zenburn-bg-05 :weight bold :background ,zenburn-orange))))
   `(diff-added                  ((t (:foreground ,zenburn-green :weight bold))))
   `(diff-removed                ((t (:foreground ,zenburn-red))))
   `(linum                       ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg))))
   '(dired-perm-write            ((t nil)))
   `(ivy-current-match           ((t (:foreground nil :background nil :underline nil))))
   '(ivy-minibuffer-match-face-1 ((t (:foreground nil :background nil :underline nil))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-red-2    :background nil))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-green+1  :background nil))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-yellow-2 :background nil))))
   `(ivy-virtual                 ((t (:inherit font-lock-type-face))))
   '(swiper-line-face            ((t (:background "#4F4F4F"))))
   '(swiper-match-face-1         ((t (:foreground nil :background nil :underline nil))))
   `(swiper-match-face-2         ((t (:foreground "white" :weight bold :background ,zenburn-red-2))))
   `(swiper-match-face-3         ((t (:foreground "white" :weight bold :background ,zenburn-green-1))))
   `(swiper-match-face-4         ((t (:foreground "white" :weight bold :background ,zenburn-yellow-2))))
   '(hl-line                     ((t (:background "#4F4F4F"))))
   ))



; 2014-04-08: local emacs overrides
(let ((local "~/.emacs.local"))
  (when (file-exists-p local) (load-file local)))
