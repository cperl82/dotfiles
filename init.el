;; Required as of emacs 25
(package-initialize)

;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "user-recipes"))

;; 2016-09-14: Testing having recipes directly inline rather than in the above directory
(setq
 el-get-sources
 (nconc
  el-get-sources
  '((:name ace-window
     :description "Quickly switch windows using `avy'"
     :type github
     :pkgname "abo-abo/ace-window"
     :checkout "77cc05f7284577ed396f292de0e7bb8ec561ea81"
     :depends (avy))

    (:name company-mode
     :website "http://company-mode.github.io/"
     :description "Modular in-buffer completion framework for Emacs"
     :type github
     :pkgname "company-mode/company-mode"
     :checkout "c494fc65d35f7f00c2da17206e6550385ae9b300")

    (:name origami.el
     :website "https://github.com/gregsexton/origami.el"
     :description "A text folding minor mode for Emacs"
     :type github
     :pkgname "gregsexton/origami.el"
     :checkout "5630536d04613476e13b413fe05fd0bbff4107ca"
     :depends (dash s)))))

(let* ((sources (map 'list (lambda (plist) (plist-get plist :name)) el-get-sources))
       (packages
        '(avy
          color-theme-zenburn
          dash
          diminish
          elisp-slime-nav
          evil
          evil-smartparens
          evil-surround
          f
          flycheck
          general
          haskell-mode
          helm
          helm-projectile
          highlight-parentheses
          hydra
          json-mode
          json-reformat
          json-snatcher
          lua-mode
          markdown-mode
          org-mode
          projectile
          rainbow-mode
          resize-window
          s
          smartparens
          smex
          swiper
          systemtap-mode
          tuareg-mode
          undo-tree
          use-package
          vagrant-tramp
          wgrep
          which-key
          xcscope
          yaml-mode
          ))
       (sources (append sources packages)))
  (el-get 'sync sources))

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

;; 2014-05-07: function to revert all buffers
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

;; 2016-09-10 better alignment of property lists
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

;; 2017-01-12 function to help split windows the way I like it
(defun cp/split-windows-sensibly (&rest r)
  (when (eq (length (window-list)) 1)
    (let* ((columns (window-size nil t))
           (new (1+ (/ columns 2))))
      (progn
        (message "Setting split-width-threshold to %d" new)
        (setq split-width-threshold new)))))

(advice-add 'split-window-sensibly :before #'cp/split-windows-sensibly)
(advice-add 'split-window-horizontally :before #'cp/split-windows-sensibly)


;; Base packages
(require 'general)
(require 'use-package)
(require 'diminish)
(require 'dash)
(require 's)

(setq cp/normal-prefix "SPC")
(setq cp/non-normal-prefix "M-SPC")

;; general default prefix key bindings
(general-define-key
 :keymaps `(motion)
  "SPC" nil
  ","   nil)

(general-define-key
 :keymaps '(normal visual motion insert emacs)
 :prefix cp/normal-prefix
 :non-normal-prefix cp/non-normal-prefix
  "a" '(:ignore t :which-key "applications")
  "b" '(:ignore t :which-key "buffers")
  "f" '(:ignore t :which-key "files")
  "w" '(:ignore t :which-key "windows")
  "f f" #'find-file
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
  "w =" #'balance-windows
  "h"   #'help-command)

;; general command prefix keybindings, normal and motion state only
(general-define-key
 :keymaps '(normal motion)
 :prefix ","
  "h" #'cp/evil-highlight-symbol
  "x" #'delete-window
  "o" #'delete-other-windows
  "s" #'split-window-vertically
  "v" #'split-window-horizontally
  "j" #'dired-jump
  "f" #'find-file
  "k" #'kill-buffer
  "K" #'kill-buffer-and-window
  "e" #'cp/escreen-get-active-screen-names-with-emphasis)



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
  (:keymaps '(normal visual motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w h" #'evil-window-left
   "w j" #'evil-window-down
   "w k" #'evil-window-up
   "w l" #'evil-window-right)
  (:keymaps '(normal motion emacs)
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "C-l" #'evil-window-right)
  (:keymaps '(visual)
   "/" #'cp/evil-search-forward
   "?" #'cp/evil-search-backward
   "i" #'indent-region)
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
;; CR-soon cperl: This can probably be replaced with smartparens
(use-package evil-surround :defer t)



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
  (:keymaps '(normal motion)
   :prefix cp/normal-prefix
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
  (:keymaps '(normal motion)
   :prefix cp/normal-prefix
   "a a w" #'ace-window))



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
(defun cp/swiper-thing-at-point ()
  (interactive)
  (swiper (thing-at-point 'symbol)))

(use-package smex
  :defer t)

(use-package ivy
  :defer t
  :diminish ivy-mode
  :general
  (:keymaps '(ivy-minibuffer-map)
   "<up>"  #'ivy-previous-history-element
   "<down" #'ivy-next-history-element)
  ("C-s"   #'counsel-grep-or-swiper)
  :init
  (progn
    (use-package ivy-buffer-extend
      :config
      (setq ivy-buffer-max-dir-display-length 50)
      (setq ivy-buffer-max-buffer-display-length 50)
      (setq ivy-buffer-format
            '(buffer-name   "<col>"  "    "
                            mode process  "<col>"  "    "
                            dir file-name "<col>"  "    ")))
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
    (counsel-mode 1)))



;; buffer-move
(use-package buffer-move
  :defer t
  :commands (buf-move-down buf-move-up buf-move-left buf-move-right)
  :init
  :general
  (:keymaps '(normal visual motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w H" #'buf-move-left
   "w J" #'buf-move-down
   "w K" #'buf-move-up
   "w L" #'buf-move-right)
  (:keymaps '(normal motion)
   :prefix nil
   "C-M-h" #'buf-move-left
   "C-M-j" #'buf-move-down
   "C-M-k" #'buf-move-up
   "C-M-l" #'buf-move-right))



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
  (:keymaps 'dired-mode-map
   :states '(normal)
   "h"   #'dired-up-directory
   "j"   #'dired-next-line
   "k"   #'dired-previous-line
   "l"   #'dired-find-alternate-file
   "n"   #'evil-search-next
   "N"   #'evil-search-previous
   "?"   #'evil-search-backward
   "G"   #'evil-goto-line
   "gg"  #'evil-goto-first-line
   "M-k" #'dired-kill-subdir
   "M-n" #'dired-next-subdir
   "M-p" #'dired-prev-subdir
   "c"   #'dired-create-directory
   "q"   #'kill-this-buffer
   "TAB" #'cp/dired-tab-dwim
   "o"   #'dired-find-file-other-window
   "r"   #'revert-buffer
   "."   #'cp/dired-toggle-hiding-dotfiles
   "SPC" nil)
  :config
  (progn
    (use-package dired-x)
    (put 'dired-find-alternate-file 'disabled nil)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))



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
   "M-k" #'cscope-history-kill-file
   "M-K" #'cscope-history-kill-result
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
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  :general
  (:keymaps '(elisp-slime-nav-mode-map)
   :states  '(normal)
   :prefix nil
   :non-normal-prefix nil
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
       (cp/sh-switch-to-indentation 8)
       (electric-indent-mode nil)))))



;; grep
(use-package grep
  :defer t
  :general
  (:keymaps 'grep-mode-map
   :states '(normal)
   "M-n" #'compilation-next-error)
  (:keymaps 'grep-mode-map
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
  (:keymaps 'Man-mode-map
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
    (evil-smartparens-mode)
    (define-key evil-normal-state-local-map
      (kbd "(")
      (lambda (&optional arg)
        (interactive "P") (sp-wrap-with-pair "(")))
    (define-key evil-normal-state-local-map (kbd "C-t") #'sp-transpose-sexp)
    (define-key evil-normal-state-local-map (kbd "M-7") #'sp-backward-barf-sexp)
    (define-key evil-normal-state-local-map (kbd "M-8") #'sp-forward-barf-sexp)
    (define-key evil-normal-state-local-map (kbd "M-9") #'sp-backward-slurp-sexp)
    (define-key evil-normal-state-local-map (kbd "M-0") #'sp-forward-slurp-sexp)
    (define-key evil-normal-state-local-map (kbd "M-s") #'sp-splice-sexp)
    (define-key evil-normal-state-local-map (kbd "M-S") #'sp-split-sexp)
    (define-key evil-normal-state-local-map (kbd "M-j") #'sp-join-sexp)
    (define-key evil-normal-state-local-map (kbd "M-n") #'sp-next-sexp)
    (define-key evil-normal-state-local-map (kbd "M-p") #'sp-previous-sexp)
    (define-key evil-normal-state-local-map (kbd "M-o") #'sp-down-sexp)
    (define-key evil-normal-state-local-map (kbd "M-u") #'sp-backward-down-sexp)
    (define-key evil-normal-state-local-map (kbd "M-l") #'sp-forward-sexp)
    (define-key evil-normal-state-local-map (kbd "M-h") #'sp-backward-sexp)
    (define-key evil-normal-state-local-map (kbd "M-k") #'sp-splice-sexp-killing-backward-or-around)
    (define-key evil-normal-state-local-map (kbd "M-K") #'sp-splice-sexp-killing-forward)))

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



;; resize window
(use-package resize-window
  :defer t
  :general
  (:keymaps '(normal insert motion visual emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w r" #'resize-window)
  :config
  (progn
    (push '(?v ?p) resize-window-alias-list)
    (push '(?V ?n) resize-window-alias-list)
    (push '(?h ?b) resize-window-alias-list)
    (push '(?H ?f) resize-window-alias-list)))



;; winner
(use-package winner
  :defer t
  :general
  (:keymaps '(normal insert motion visual emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "w u" #'winner-undo
   "w U" #'winner-redo)
  :init
  (winner-mode))



;; escreen
(defun cp/escreen-swap-screen (other-screen-number)
  (if (and
       (numberp other-screen-number)
       (not (eq other-screen-number escreen-current-screen-number)))
      (let ((screen-data-current (escreen-configuration-escreen escreen-current-screen-number))
            (screen-data-other   (escreen-configuration-escreen other-screen-number)))
        (cond ((and screen-data-current screen-data-other)
               ; The other screen does exist
               (progn
                 (setcar screen-data-current other-screen-number)
                 (setcar screen-data-other   escreen-current-screen-number)
                 (setq escreen-current-screen-number other-screen-number)))
              ((and screen-data-current (not screen-data-other))
               ; The other screen doesn't exist
               (progn
                 (setcar screen-data-current other-screen-number)
                 (setq escreen-current-screen-number other-screen-number)))))))

(defun cp/escreen-move-screen (direction)
  (let ((other-screen-number
         (cond ((eq direction 'left)  (1- escreen-current-screen-number))
               ((eq direction 'right) (1+ escreen-current-screen-number)))))
    (cond ((and
            (>= other-screen-number 0)
            (<= other-screen-number escreen-highest-screen-number-used))
           (cp/escreen-swap-screen other-screen-number))
          ; These are the cases where we're moving right off the right
          ; end or left off the left end
          ; TODO: some of the below can probably be factored out
          ((< other-screen-number 0)
           (let ((n 1)
                 (end escreen-highest-screen-number-used))
            (while (<= n end)
              (cp/escreen-swap-screen n)
              (setq n (1+ n)))))
          ((> other-screen-number escreen-highest-screen-number-used)
           (let ((n (1- escreen-highest-screen-number-used)))
             (while (>= n 0)
               (cp/escreen-swap-screen n)
               (setq n (1- n)))))))
  (cp/escreen-get-active-screen-names-with-emphasis))

(defun cp/escreen-move-screen-left ()
  (interactive)
  (cp/escreen-move-screen 'left))

(defun cp/escreen-move-screen-right ()
  (interactive)
  (cp/escreen-move-screen 'right))

(defun cp/escreen-rename-screen (&optional name number suppress-message)
  (interactive "sNew screen name: ")
  (let ((screen-data (escreen-configuration-escreen (or number escreen-current-screen-number)))
        (new-name (cond ((equal name "") nil)
                        ((stringp name) name)
                        (t "default"))))
    (setcar (cdr screen-data) new-name)
    (when (not suppress-message)
      (cp/escreen-get-active-screen-names-with-emphasis))))

(defun cp/escreen-get-active-screen-names-with-emphasis ()
  (interactive)
  (let ((output ""))
    (dolist (n (escreen-get-active-screen-numbers))
      (let* ((data (escreen-configuration-escreen n))
             (screen-name (nth 1 data))
             (star (propertize "*" 'face 'font-lock-string-face)))
        (setq output
              (format "%s  %s" output
                      (cond ((and
                              (eq escreen-current-screen-number n) screen-name)
                             (format "%d%s %s" n star screen-name))
                            ((eq escreen-current-screen-number n) (format "%d%s" n star))
                            (screen-name (format "%d- %s" n screen-name))
                            (t (format "%d-" n))))))
      (message "escreen: active screens: %s" output))))

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
  :commands (escreen-get-active-screen-numbers)
  :general
  (:keymaps '(normal)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a e" '(:keymap escreen-map :which-key "escreen"))
  (:keymaps 'escreen-map
   "E" #'cp/escreen-get-active-screen-names-with-emphasis
   "r" #'cp/escreen-rename-screen
   "l" #'escreen-goto-next-screen
   "h" #'escreen-goto-prev-screen)
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

(setq cp/org-helm-username-list-command-alist
      '((darwin     . "dscacheutil -q user | awk -F: '$1 ~ /name/ {print $2}'")
        (gnu/linux  . "getent passwd | cut -d: -f1")))

(setq cp/org-list-all-usernames
 (lexical-let ((expire 86400)
               (cache `(,(current-time) . nil)))
   (lambda ()
     (let* ((cmd (assq system-type cp/org-helm-username-list-command-alist))
            (cached-at (car cache))
            (usernames (cdr cache))
            (now (current-time))
            (diff (time-subtract now cached-at)))
       (when cmd
         (let ((cmd (cdr cmd)))
           (if (or (not usernames) (> (time-to-seconds diff) expire))
               (progn
                 (message
                  "Org Helm username cache older than %ds, cached at %s, expiring"
                  expire
                  (current-time-string cached-at))
                 (let ((result (split-string (shell-command-to-string cmd))))
                   (setq cache `(,now . ,result))
                   result))
             usernames)))))))

(defun cp/org-list-all-usernames ()
  (funcall cp/org-list-all-usernames))

(defun cp/org-helm-usernames (username)
  (helm :input username
        :candidate-number-limit nil
        :fuzzy-match t
        :sources
        (helm-build-sync-source "usernames" :candidates (cp/org-list-all-usernames))
        :buffer "*helm usernames*"))

(defun cp/org-helm-complete-user-name-at-point ()
  (interactive)
  (let* ((partial  (word-at-point))
         (username (cp/org-helm-usernames partial)))
    (when username
      (progn
        (when partial
          (backward-kill-word 1))
        (insert username)))))

(defun cp/advice/org-next-link (&optional search-backward)
  (cp/org-echo-link-at-point))

(defun cp/advice/org-previous-link ()
  (cp/org-echo-link-at-point))

;; http://emacs.stackexchange.com/questions/9585/org-how-to-sort-headings-by-todo-and-then-by-priority
(defun cp/todo-to-int (todo)
  (car
   (-non-nil
    (mapcar
     (lambda (keywords)
       (let* ((finished-states
               (cdr (-drop-while (lambda (x) (not (string= x "|"))) keywords)))
              (n-finished-states (length finished-states))
              (n-finished-states (if (eq n-finished-states 0) 1 n-finished-states))
              (todo-seq-orig
               (-map (lambda (x) (car (split-string x "(")))
                     (cdr keywords)))
              (todo-seq-mod (-rotate n-finished-states todo-seq-orig)))
         (-find-index (lambda (x) (string= x todo)) todo-seq-mod)))
     org-todo-keywords))))

(defun cp/non-todo-to-int (max todo)
  (let ((item (replace-regexp-in-string "^\\** *" "" todo)))
    (if (string-match "\\bNotes\\b" item) -1 max)))

(defun cp/org-sort-key ()
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int
          (if todo
              (cp/todo-to-int todo)
            (cp/non-todo-to-int todo-max (org-entry-get (point) "ITEM"))))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)))

(defun cp/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'cp/org-sort-key)
  ;; there may be a better way to do this, but for now its refolds things the way I want after sorting
  (funcall (general-simulate-keys "TAB TAB")))

(use-package org
  :defer t
  :general
  (:keymaps '(org-mode-map org-agenda-mode-map)
   :states  '(normal emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "o"   '(:ignore t :which-key "org")
   "o p" #'org-previous-link
   "o n" #'org-next-link
   "o a" #'org-agenda
   "o t" #'org-todo
   "o T" #'org-set-tags
   "o P" #'org-set-property
   "o s" #'cp/org-sort-entries)
  (:keymaps '(org-mode-map)
   :states  '(normal)
   "TAB"     #'org-cycle
   "M-h"     #'org-metaleft
   "M-l"     #'org-metaright
   "M-k"     #'org-metaup
   "M-j"     #'org-metadown
   "M-H"     #'org-shiftmetaleft
   "M-L"     #'org-shiftmetaright
   "M-K"     #'org-shiftmetaup
   "M-J"     #'org-shiftmetadown
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
  (:keymaps '(org-mode-map)
   :states  '(insert)
   "M-RET"   #'org-meta-return
   "M-."     #'cp/org-surround-tilda
   "M-v"     #'cp/org-surround-equal
   "M-b"     #'cp/org-surround-star
   "M-u"     #'cp/org-helm-complete-user-name-at-point)
  (:keymaps '(org-agenda-mode-map)
   :states  '(emacs)
   "j"       #'org-agenda-next-line
   "k"       #'org-agenda-previous-line
   "h"       #'left-char
   "l"       #'right-char
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture)
  :config
  (progn
    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))
    (use-package org-depend)
    (setq org-tags-column -90)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-todo-keywords
          '((sequence "DFER(r)" "DPND(x)" "WAIT(w)" "NEXT(n)" "|" "DONE(d)" "CNCL(c)")))
    (setq org-todo-keyword-faces
          '(("DFER" . "#767676")
            ("DPND" . "#767676")
            ("WAIT" . "#8C5353")
            ("CNCL" . "#FFFFFF")
            ("DONE" . "#FFFFFF")))
    (setq org-agenda-files '("~/org"))
    (setq org-agenda-tags-column -90)
    (setq org-agenda-custom-commands
          `(("r" "Read/Review                 "
             ((tags-todo "read/NEXT"
                         ((org-agenda-overriding-header "NEXT ACTIONS, Read/Review")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("d" "Deferred (with reminder)    "
             ((tags-todo "DEADLINE={.+}/DFER"
                         ((org-agenda-overriding-header "DEFERRED, with reminder")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("D" "Deferred (without reminder) "
             ((tags-todo "-DEADLINE={.+}/DFER"
                         ((org-agenda-overriding-header "DEFERRED, without reminder")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("n" . "Next actions")
            ("na" "NEXT action (all)          "
             ((tags-todo "/NEXT"
                         ((org-agenda-overriding-header "NEXT ACTIONS, ALL")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("nt" "NEXT action by tag         "
             ((tags-todo ""
                         ((org-agenda-overriding-header "NEXT ACTIONS")
                          (org-agenda-sorting-strategy '(tsia-up))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT")))))))
            ("w" . "Waiting for")
            ("wa" "WAIT for (all)             "
             ((tags-todo "/WAIT"
                         ((org-agenda-overriding-header "WAITING FOR (all))")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("wt" "WAIT for by tag            "
             ((tags-todo ""
                         ((org-agenda-overriding-header "WAITING FOR by tag")
                          (org-agenda-sorting-strategy '(tsia-up))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WAIT")))))))
            ("x" . "Stuck")
            ("xn" "NEXT action, no deadline   "
             ((tags-todo "-DEADLINE={.+}/NEXT"
                         ((org-agenda-overriding-header "NEXT action, no deadline")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("xw" "WAIT for, no deadline      "
             ((tags-todo "-DEADLINE={.+}/WAIT"
                         ((org-agenda-overriding-header "WAIT for, no deadline")
                          (org-agenda-sorting-strategy '(tsia-up))))))
            ("u" "Untagged next actions       "
             ((tags-todo "-{.*}"
                         ((org-agenda-overriding-header "NEXT ACTIONS, no context")
                          (org-agenda-sorting-strategy '(tsia-up))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT")))))))))
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
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
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
         (turn-on-evil-surround-mode)
         (add-hook 'write-contents-functions (lambda () (save-excursion (delete-trailing-whitespace)))))))
    (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))
    (add-hook 'org-src-mode-hook    (lambda () (setq electric-indent-mode nil)))))



; helm
; 2015-09-11 Ripped wholesale from helm-buffers.el so I could control the formatting of dir
(defun cp/advice/helm-buffer--show-details
    (orig-fun buf-name prefix help-echo size mode dir face1 face2 proc details type)
  (if (projectile-project-p)
      (let* ((regex (format "^.*\\(%s.*\\)$" (projectile-project-name)))
             (dir (replace-regexp-in-string regex "\\1" dir)))
        (append
         (list
          (concat prefix
                  (propertize buf-name 'face face1
                              'help-echo help-echo
                              'type type)))
         (and details
              (list size mode
                    (propertize
                     (if proc
                         (format "(%s %s in `%s')"
                                 (process-name proc)
                                 (process-status proc) dir)
                       (format "%s" dir))
                     'face face2)))))
    (apply orig-fun buf-name prefix help-echo size mode dir face1 face2 proc details type ())))

(use-package helm
  :defer t
  :general
  (:keymaps '(normal visual motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a h"   '(:ignore t :which-key "helm")
   "a h m" #'helm-mini
   "a h f" #'helm-find-files
   "a h F" #'helm-find
   "a h b" #'helm-buffers-list
   "a h o" #'helm-occur
   "a h a" #'helm-apropos
   "a h r" #'helm-resume)
  (:keymaps 'helm-map
   "TAB" #'helm-execute-persistent-action
   "C-z" #'helm-select-action)
  :config
  (progn
    (advice-add 'helm-buffer--show-details :around #'cp/advice/helm-buffer--show-details)
    (setq helm-split-window-default-side 'right)))



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
  (:keymaps '(normal visual motion insert emacs)
   :prefix nil
   :non-normal-prefix nil
   "C-c p" '(:keymap projectile-command-map :which-key "projectile"))
  (:keymaps '(normal visual motion insert emacs)
   :prefix cp/normal-prefix
   :non-normal-prefix cp/non-normal-prefix
   "a p"   '(:keymap projectile-command-map :which-key "projectile"))
  :init
  (progn
    (advice-add 'projectile-serialize              :around #'cp/advice/projectile-serialize)
    (advice-add 'projectile-unserialize            :around #'cp/advice/projectile-unserialize)
    (advice-add 'projectile-maybe-invalidate-cache :around #'cp/advice/projectile-maybe-invalidate-cache))
  :config
  (progn
    (setq projectile-enable-caching t)
    (add-to-list 'projectile-project-root-files-bottom-up "cscope.files")
    (projectile-global-mode)

    (use-package helm-projectile
      :init
      (setq helm-projectile-fuzzy-match nil)
      :config
      (progn
        (helm-projectile-on)
        (setq projectile-switch-project-action #'helm-projectile)))))



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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" default)))
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
   '(helm-buffer-directory       ((t (:foreground "color-247"))))
   '(helm-ff-dotted-directory    ((t (:foreground "color-247"))))
   `(helm-match                  ((t (:foreground ,zenburn-red-2 :weight normal))))
   `(ivy-current-match           ((t (:foreground nil :background nil :underline nil))))
   '(ivy-minibuffer-match-face-1 ((t (:foreground nil :background nil :underline nil))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-red-2    :background nil))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-green+1  :background nil))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-yellow-2 :background nil))))
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
