(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror) (with-current-buffer
    (url-retrieve-synchronously
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "user-recipes"))

(el-get
 'sync
 '(color-theme-zenburn
   diminish
   evil
   evil-leader
   flx
   haskell-mode
   helm
   helm-projectile
   ido-vertical-mode
   lua-mode
   neotree
   org-mode
   projectile
   rainbow-mode
   s
   systemtap-mode
   tuareg-mode
   undo-tree
   use-package
   which-key
   xcscope
   xoria256-emacs))

; 2014-04-26: Loading other stuff
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'use-package)
(require 'diminish)

; misc settings
(setq
 inhibit-startup-message t
 column-number-mode      t
 split-height-threshold  nil
 make-backup-files       nil)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; 2014-05-07: function to revert all buffers
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

; 2014-04-22 mode-line-format
(setq mode-line-format
      '("%e"
	mode-line-front-space
	mode-line-mule-info
	mode-line-client
	mode-line-modified
	mode-line-remote
	mode-line-frame-identification
	mode-line-buffer-identification
	"   "
	(:eval (cp/format-default-dir-for-mode-line default-directory 40))
	"   "
	mode-line-position
	evil-mode-line-tag
	(vc-mode vc-mode)
	"  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

; 2015-09-11 Enable narrowing command which are disabled by default
(put 'narrow-to-region 'disabled nil)

; Remove the binding to compose mail, I don't use it
(global-set-key (kbd "C-x m")   nil)

; 2014-03-27: Always show matching parents
(setq show-paren-delay 0)
(show-paren-mode)


;; uniquify
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'forward)))


;; which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (progn
    (which-key-mode)))


; evil
; 2014-04-01: http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :diminish (undo-tree-mode . "UT")
  :init
  (progn
    (setq-default evil-symbol-word-search t)
    (setq-default evil-flash-delay 5))
  :config
  (progn
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map  [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
    (evil-mode 1)))


; evil-leader
(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" 'find-file
      "b" 'switch-to-buffer
      "s" 'split-window-vertically
      "v" 'split-window-horizontally
      "k" 'kill-buffer
      "K" 'kill-buffer-and-window
      "o" 'delete-other-windows
      "x" 'delete-window
      "e" 'cp/escreen-get-active-screen-names-with-emphasis
      "H" 'help-command
      "h" 'cp-evil-highlight-symbol
      "R" 'revert-buffer-all
      "E" '(lambda () (interactive) (message (buffer-file-name))))))

; 2014-03-28: Functions to support selecting something in Visual mode
; and then automatically start searching for it by pressing "/" or "?"
(defun cp-evil-highlight-symbol ()
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

(evil-define-operator cp-evil-search (beg end forward)
  (let* ((search-string (buffer-substring-no-properties beg end))
	 (quoted-string (regexp-quote search-string)))
    (setq isearch-forward forward)
    (evil-search quoted-string forward t)))

(evil-define-operator cp-evil-search-forward (beg end type)
  (cp-evil-search beg end t))

(evil-define-operator cp-evil-search-backward (beg end type)
  (cp-evil-search beg end nil))

(define-key evil-visual-state-map "/" 'cp-evil-search-forward)

(define-key evil-visual-state-map "?" 'cp-evil-search-backward)


;; tuareg-mode customizations
(defun cp/tuareg-mode-forward-sexp-fun (arg)
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
(defun cp/tuareg-mode-forward-sexp-fun-alt (arg)
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

(add-to-list
 'hs-special-modes-alist
 `(tuareg-mode
   ,(mapconcat
     'identity
     '("\\<module\\>\\s-+\\S-+\\s-+=\\s-+\\<struct\\>"
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
     "\\|")
   nil nil  cp/tuareg-mode-forward-sexp-fun))


; ido / ido-vertical-mode / flx
(use-package ido
  :config
  (progn
    (setq ido-enable-flex-matching t)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)
    ;; 2015-09-20: I never want ido-find-files to auto-merge, drives me nuts
    (setq ido-auto-merge-work-directories-length -1)
    (setq ido-enter-matching-directory 'first)
    (ido-mode t)
    (ido-everywhere t)
    (use-package ido-vertical-mode
      :config
      (progn
	(setq-default ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
	(ido-vertical-mode 1)))
    (use-package flx-ido
      :config
      (flx-ido-mode t))))


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
  :bind-keymap ("C-\\" . escreen-map)
  :config
  (progn
    (advice-add 'escreen-goto-screen   :after #'cp/advice/escreen-goto-screen)
    (advice-add 'escreen-kill-screen   :after #'cp/advice/escreen-kill-screen)
    (advice-add 'escreen-create-screen :after #'cp/advice/escreen-create-screen)
    (advice-add 'escreen-install       :after #'cp/advice/escreen-install)
    (escreen-install)
    (define-key escreen-map (kbd "r")    'cp/escreen-rename-screen)))


; 2014-04-24: hide show related
; 2014-04-30: I'm not sure why the hook works but the `evil-define-key' doesn't (well, I
; mean it sort of works in that if I enter insert mode and then exit back into normal mode
; the keybinding will be there, but I want it to just be there right away).
(add-hook
 'hs-minor-mode-hook
 (lambda ()
   (cond ((eq major-mode 'Man-mode)
	  (define-key evil-motion-state-local-map (kbd "TAB") 'hs-toggle-hiding))
	 (t
	  (define-key evil-normal-state-local-map (kbd "TAB") 'hs-toggle-hiding)))))


;; dired
(use-package dired
  :defer t
  :config
  (progn
    (use-package dired-x)
    (evil-define-key 'normal dired-mode-map (kbd "TAB")  'dired-hide-subdir)
    (evil-define-key 'normal dired-mode-map (kbd "n")    'evil-search-next)
    (evil-define-key 'normal dired-mode-map (kbd "N")    'evil-search-previous)
    (evil-define-key 'normal dired-mode-map (kbd "?")    'evil-search-backward)
    (evil-define-key 'normal dired-mode-map (kbd "G")    'evil-goto-line)
    (evil-define-key 'normal dired-mode-map (kbd "gg")   'evil-goto-first-line)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))


;; org
(defun cp/echo-link-at-point-if-not-darwin ()
  (when (not (string-equal system-type "darwin"))
    (let* ((el (org-element-context))
           (raw-link (plist-get (cadr el) :raw-link)))
      (message "%s" raw-link))))

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

(use-package org
  :config
  (progn
    (add-hook
     'org-mode-hook
     (lambda ()
       (progn
	 (auto-fill-mode)
	 (setq fill-column 90)
	 (setq indent-tabs-mode nil)
	 (add-hook
	  'write-contents-functions
	  (lambda ()
	    (save-excursion
	      (delete-trailing-whitespace)))))))
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-files '("~/org"))
    (setq org-capture-templates
	  '(("n" "Next Action" entry (file "~/org/capture.org") "* NEXT  %?\n")
	    ("N" "Next Action with Gmail Id" entry (file "~/org/capture.org") "* NEXT  %?\n  [[gmail:%^{gmail id}][%\\1]]")
	    ("p" "Project" entry (file "~/org/capture.org") "*  %?\n")
	    ("P" "Project with Gmail Id" entry (file "~/org/capture.org") "* %?\n  [[gmail:%^{gmail id}][%\\1]]")))
    (setq org-todo-keywords
	  '((sequence "NEXT(n)" "DPND(x)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)")
	    (sequence "DFER(r)" "|" "DONE(d)" "CNCL(c)")))
    (setq org-todo-keyword-faces
	  '(("DFER" . "#767676")
	    ("DPND" . "#767676")
	    ("WAIT" . "#8C5353")
	    ("CNCL" . "#FFFFFF")
	    ("DONE" . "#FFFFFF")))
    (setq org-link-abbrev-alist
	  '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
    (setq org-agenda-custom-commands
	  `(("d" "Deferred (with tickler)    "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "DEADLINE={.+}+TODO=\"DFER\""
			 ((org-agenda-overriding-header "DEFERRED, with tickler")
			  (org-agenda-sorting-strategy '(priority-down))))))
	    ("D" "Deferred (without tickler) "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "-DEADLINE={.+}+TODO=\"DFER\""
			 ((org-agenda-overriding-header "DEFERRED, without tickler")
			  (org-agenda-sorting-strategy '(priority-down))))))
	    ("r" "Read/Review                "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "read+TODO=\"NEXT\""
			 ((org-agenda-overriding-header "NEXT ACTIONS, Read/Review")
			  (org-agenda-sorting-strategy '(priority-down))))))
	    ("N" "NEXT ACTION (all)          "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "TODO=\"NEXT\""
			 ((org-agenda-overriding-header "NEXT ACTIONS, ALL")
			  (org-agenda-sorting-strategy '(priority-down))))))
	    ("n" "NEXT ACTION by tag         "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo ""
			 ((org-agenda-overriding-header "NEXT ACTIONS")
			  (org-agenda-sorting-strategy '(priority-down))
			  (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT")))))))
	    ("W" "WAITING FOR (all)          "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "TODO=\"WAIT\""
			 ((org-agenda-overriding-header "WAITING FOR")
			  (org-agenda-sorting-strategy '(priority-down))))))
	    ("w" "WAITING FOR by tag         "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo ""
			 ((org-agenda-overriding-header "WAITING FOR")
			  (org-agenda-sorting-strategy '(priority-down))
			  (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WAIT")))))))
	    ("u" "Untagged                   "
	     ((agenda "" ((org-agenda-span 1)
			  (org-deadline-warning-days 1)))
	      (tags-todo "-{.*}"
			 ((org-agenda-overriding-header "NEXT ACTIONS, no context")
			  (org-agenda-sorting-strategy '(priority-down))
			  (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT")))))))))
    (setq org-tags-column -120)
    (setq org-agenda-tags-column -120)
    (setq org-refile-use-outline-path 'file)
    (setq org-hide-block-startup t)
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-catch-invisible-edits 'error)
    (setq org-ctrl-k-protect-subtree t)
    (setq org-cycle-include-plain-lists 'integrate)
    (add-to-list 'org-open-at-point-functions 'cp/echo-link-at-point-if-not-darwin)
    (setq org-make-link-description-function 'cp/org-link-auto-desc-from-abbrev-tags)
    (evil-define-key 'normal org-mode-map        (kbd "TAB")   'org-cycle)
    (evil-define-key 'normal org-mode-map        (kbd "M-h")   'org-metaleft)
    (evil-define-key 'normal org-mode-map        (kbd "M-l")   'org-metaright)
    (evil-define-key 'normal org-mode-map        (kbd "M-k")   'org-metaup)
    (evil-define-key 'normal org-mode-map        (kbd "M-j")   'org-metadown)
    (evil-define-key 'normal org-mode-map        (kbd "M-H")   'org-shiftmetaleft)
    (evil-define-key 'normal org-mode-map        (kbd "M-L")   'org-shiftmetaright)
    (evil-define-key 'normal org-mode-map        (kbd "M-K")   'org-shiftmetaup)
    (evil-define-key 'normal org-mode-map        (kbd "M-J")   'org-shiftmetadown)
    (evil-define-key 'normal org-mode-map        (kbd "C-c a") 'org-agenda)
    (evil-define-key 'normal org-mode-map        (kbd "C-c c") 'org-capture)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "j")     'org-agenda-next-line)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "k")     'org-agenda-previous-line)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "h")     'left-char)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "l")     'right-char)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd " ")     'org-agenda-cycle-show)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-c a") 'org-agenda)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-c c") 'org-capture)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-h")   'evil-window-left)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-l")   'evil-window-right)))


;; buffer-move
(use-package buffer-move
  :commands (buf-move-down buf-move-up buf-move-left buf-move-right)
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-M-j") 'buf-move-down)
    (define-key evil-normal-state-map (kbd "C-M-k") 'buf-move-up)
    (define-key evil-normal-state-map (kbd "C-M-h") 'buf-move-left)
    (define-key evil-normal-state-map (kbd "C-M-l") 'buf-move-right)
    (define-key evil-motion-state-map (kbd "C-M-j") 'buf-move-down)
    (define-key evil-motion-state-map (kbd "C-M-k") 'buf-move-up)
    (define-key evil-motion-state-map (kbd "C-M-h") 'buf-move-left)
    (define-key evil-motion-state-map (kbd "C-M-l") 'buf-move-right)))


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
  :config
  (progn
    (add-to-list
     'hs-special-modes-alist
     `(Man-mode
       ,Man-heading-regexp
       nil
       nil
       cp/man-forward-sexp-fun))
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
	nil	     ; Copied from /usr/share/vim/vim74/syntax/man.vim
	`((,Man-heading-regexp          . font-lock-comment-face)
	  ("^\\s-*[+-][a-zA-Z0-9]\\S-*" . font-lock-function-name-face)
	  ("^\\s-*--[a-zA-Z0-9-]\\S-*"  . font-lock-function-name-face))
	'set)
       (font-lock-mode 1)))))


;; xcscope
(use-package xcscope
  :defer t
  :config
  (progn
    (setq cscope-option-use-inverted-index t)
    (setq cscope-edit-single-match nil)
    (setq cscope-option-kernel-mode t)
    (add-hook
     'cscope-list-entry-hook
     (lambda ()
       (setq-local face-remapping-alist
		   '((cscope-separator-face   font-lock-string-face)
		     (cscope-line-number-face font-lock-string-face)
		     (cscope-file-face        font-lock-doc-face)
		     (cscope-function-face    font-lock-function-name-face)))
       (define-key evil-normal-state-local-map (kbd "RET") 'cscope-select-entry-inplace)
       (define-key evil-normal-state-local-map (kbd "SPC") 'cscope-show-entry-other-window)
       (define-key evil-normal-state-local-map (kbd "TAB") 'cscope-select-entry-other-window)
       (define-key evil-normal-state-local-map (kbd   "q") 'cscope-bury-buffer)
       (define-key evil-normal-state-local-map (kbd "M-n") 'cscope-history-forward-line)
       (define-key evil-normal-state-local-map (kbd "M-p") 'cscope-history-backward-line)
       (define-key evil-normal-state-local-map (kbd "M-k") 'cscope-history-kill-result)))))

(setq c-default-style "linux")

; 2014-04-04: Holy moly its effort to get line numbers like vim!
; http://www.emacswiki.org/emacs/LineNumbers#toc6
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'linum)))

(unless window-system
  (setq linum-format 'linum-format-func))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("799291799f87afb7a2a55bd63082c58fb58912bee0a6e3d5c1ce0e083ed046c9" default))))

; 2014-12-06 override zenburn's default isearch highlighting
(custom-theme-set-faces
 `zenburn
 `(isearch                  ((t (:foreground ,"#383838" :weight bold :background ,"#DFAF8F"))))
 `(lazy-highlight           ((t (:foreground ,"#383838" :weight bold :background ,"#DFAF8F"))))
 `(diff-added               ((t (:foreground, "#7F9F7F" :weight bold))))
 `(diff-removed             ((t (:foreground, "#CC9393"))))
 '(dired-perm-write         ((t nil)))
 '(flx-highlight-face       ((t (:foreground "#CC9393" :weight normal))))
 '(helm-buffer-directory    ((t (:foreground "color-247"))))
 '(helm-ff-dotted-directory ((t (:foreground "color-247"))))
 '(helm-match               ((t (:foreground "gold1" :weight normal)))))

; 2014-12-07 Trying to make sh-mode indentation better
; Copied from http://keramida.wordpress.com/2008/08/08/tweaking-shell-script-indentation-in-gnu-emacs
(defun cp/setup-sh-mode ()
    "My own personal preferences for `sh-mode'.

This is a custom function that sets up the parameters I usually
prefer for `sh-mode'.  It is automatically added to
`sh-mode-hook', but is can also be called interactively."
    (interactive)
    (progn
      (setq sh-basic-offset 8
	    sh-indentation 8)
      ; TODO: Do you want to enable electric-indent-mode for everything?
      (electric-indent-mode nil)))

(add-hook 'sh-mode-hook 'cp/setup-sh-mode)

(require 's)
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
  :config
  (progn
    (advice-add 'helm-buffer--show-details :around #'cp/advice/helm-buffer--show-details)
    (setq helm-split-window-default-side 'right)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")   'helm-select-action)))


;; grep
(use-package grep
  :defer t
  :config
  (progn
    (setq grep-find-use-xargs 'gnu)
    (evil-define-key 'normal grep-mode-map (kbd "TAB") 'compilation-display-error)
    (add-to-list 'grep-files-aliases '("ml"  . "*.ml *.mli"))
    (add-to-list 'grep-files-aliases '("mlc" . "*.ml *.mli *.c *.h"))))


;; projectile
; 2016-04-25: Advice for the low level projectile functions that manage the cache so I can
; track (roughly) when a project was cached and invalidate the cache if I determine there
; is a good reason (e.g. ".hg/dirstate" is newer than the time the projects file were
; cached). In addition, don't serialize to disk, only keep the cache in memory.
(setq cp/projectile-projects-cache-by-time (make-hash-table :test 'equal))

(defun cp/projectile-projects-cache-by-time-sync (data)
  (let* ((projectile-keys (projectile-hash-keys data))
	 (time-keys (projectile-hash-keys cp/projectile-projects-cache-by-time))
	 (keys-to-add (set-difference projectile-keys time-keys))
	 (keys-to-delete (set-difference time-keys projectile-keys)))
    (progn
      (dolist (project keys-to-add)
	(puthash project (current-time) cp/projectile-projects-cache-by-time))
      (dolist (project keys-to-delete)
	(remhash project cp/projectile-projects-cache-by-time)))))

(defun cp/advice/projectile-serialize (orig-fun data filename)
  (cond ((eq filename projectile-cache-file) (cp/projectile-projects-cache-by-time-sync data))
        ((eq filename projectile-known-projects-file) nil)
        (t (apply orig-fun data filename ()))))

(defun cp/advice/projectile-unserialize (orig-fun filename)
  (cond ((eq filename projectile-cache-file) nil)
        ((eq filename projectile-known-projects-file) (projectile-hash-keys cp/projectile-projects-cache-by-time))
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
  :bind-keymap
  ("C-c p" . projectile-mode-map)
  :init
  (progn
    (advice-add 'projectile-serialize              :around #'cp/advice/projectile-serialize)
    (advice-add 'projectile-unserialize            :around #'cp/advice/projectile-unserialize)
    (advice-add 'projectile-maybe-invalidate-cache :around #'cp/advice/projectile-maybe-invalidate-cache))
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)
    (add-to-list 'projectile-project-root-files-bottom-up "cscope.files")
    (projectile-global-mode)

    (use-package helm-projectile
      :init
      (setq helm-projectile-fuzzy-match nil)
      :config
      (helm-projectile-on))))


; 2014-04-08: local emacs overrides
(let ((local "~/.emacs.local"))
  (when (file-exists-p local) (load-file local)))
