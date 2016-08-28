(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(unless (require 'el-get nil 'noerror) (with-current-buffer
    (url-retrieve-synchronously
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "user-recipes"))

(el-get
 'sync
 '(ace-jump-mode
   color-theme-zenburn
   diminish
   elisp-slime-nav
   evil
   evil-leader
   evil-smartparens
   evil-surround
   f
   flx
   haskell-mode
   helm
   helm-org-rifle
   helm-projectile
   highlight-parentheses
   ido-vertical-mode
   lua-mode
   neotree
   org-mode
   projectile
   rainbow-mode
   rainbow-delimiters
   s
   smartparens
   systemtap-mode
   tuareg-mode
   undo-tree
   use-package
   which-key
   xcscope
   xoria256-emacs))

; 2014-04-26: Loading other stuff
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(defun cp/confirm-before-quit (force)
  "Ask for confirmation before quiting emacs.  If a prefix argument is
given, it skips the confirmation"
  (interactive "P")
  (when (or force (y-or-n-p "Really quit emacs? ")) (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") #'cp/confirm-before-quit)

; misc settings
(setq
 inhibit-startup-message t
 column-number-mode      t
 split-height-threshold  nil
 make-backup-files       nil
 c-default-style         "linux"
 ad-redefinition-action  'accept)

(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
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

; 2014-04-22 mode-line-format
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

; 2015-09-11 Enable narrowing command which are disabled by default
(put 'narrow-to-region 'disabled nil)

; Remove the binding to compose mail, I don't use it
(global-set-key (kbd "C-x m")   nil)

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

(require 'use-package)
(require 'diminish)
(require 'dash)

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

; 2014-03-28: Functions to support selecting something in Visual mode
; and then automatically start searching for it by pressing "/" or "?"
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
    (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
    (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)
    (define-key evil-visual-state-map (kbd "/")   #'cp/evil-search-forward)
    (define-key evil-visual-state-map (kbd "?")   #'cp/evil-search-backward)
    (evil-mode 1)))


; evil-leader
(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" #'find-file
      "b" #'switch-to-buffer
      "s" #'split-window-vertically
      "v" #'split-window-horizontally
      "k" #'kill-buffer
      "K" #'kill-buffer-and-window
      "o" #'delete-other-windows
      "x" #'delete-window
      "e" #'cp/escreen-get-active-screen-names-with-emphasis
      "H" #'help-command
      "h" #'cp/evil-highlight-symbol
      "R" #'revert-buffer-all
      "j" #'dired-jump
      "E" #'(lambda () (interactive) (message (buffer-file-name))))))


;; evil-surround
(use-package evil-surround
  :defer t)


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
    (define-key escreen-map (kbd "r") #'cp/escreen-rename-screen)
    (define-key escreen-map (kbd "l") #'escreen-goto-next-screen)
    (define-key escreen-map (kbd "h") #'escreen-goto-prev-screen)))


;; org
(defun cp/org-echo-link-at-point ()
  (let* ((el (org-element-context))
         (raw-link (plist-get (cadr el) :raw-link)))
    (message "%s" raw-link)))

(defun cp/org-echo-link-at-point-if-not-darwin ()
  (when (not (equal system-type 'darwin))
    (cp/org-echo-link-at-point)))

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
                 (message "Org Helm username cache older than %ds, cached at %s, expiring" expire (current-time-string cached-at))
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

(use-package org
  :config
  (progn
    ; http://emacs.stackexchange.com/questions/9585/org-how-to-sort-headings-by-todo-and-then-by-priority
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
      (org-sort-entries nil ?f #'cp/org-sort-key))

    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-files '("~/org"))
    (setq org-capture-templates
	  '(("n" "Next Action" entry
	     (file "~/org/capture.org") "* NEXT  %?\n  captured: %U"
	     :empty-lines 1)
	    ("N" "Next Action with Gmail Id" entry
	     (file "~/org/capture.org") "* NEXT  %?\n  captured: %U\n  [[gmail:%^{gmail id}][%\\1]]"
	     :empty-lines 1)))
    (setq org-todo-keywords
	  '((sequence "DFER(r)" "DPND(x)" "WAIT(w)" "NEXT(n)" "|" "DONE(d)" "CNCL(c)")))
    (setq org-todo-keyword-faces
	  '(("DFER" . "#767676")
	    ("DPND" . "#767676")
	    ("WAIT" . "#8C5353")
	    ("CNCL" . "#FFFFFF")
	    ("DONE" . "#FFFFFF")))
    (setq org-link-abbrev-alist '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
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
    (setq org-hide-leading-stars t)
    (setq org-make-link-description-function  #'cp/org-link-auto-desc-from-abbrev-tags)
    (add-to-list 'org-open-at-point-functions #'cp/org-echo-link-at-point-if-not-darwin)
    (evil-define-key 'normal org-mode-map        (kbd "TAB")     #'org-cycle)
    (evil-define-key 'normal org-mode-map        (kbd "M-h")     #'org-metaleft)
    (evil-define-key 'normal org-mode-map        (kbd "M-l")     #'org-metaright)
    (evil-define-key 'normal org-mode-map        (kbd "M-k")     #'org-metaup)
    (evil-define-key 'normal org-mode-map        (kbd "M-j")     #'org-metadown)
    (evil-define-key 'normal org-mode-map        (kbd "M-H")     #'org-shiftmetaleft)
    (evil-define-key 'normal org-mode-map        (kbd "M-L")     #'org-shiftmetaright)
    (evil-define-key 'normal org-mode-map        (kbd "M-K")     #'org-shiftmetaup)
    (evil-define-key 'normal org-mode-map        (kbd "M-J")     #'org-shiftmetadown)
    (evil-define-key 'normal org-mode-map        (kbd "C-c a")   #'org-agenda)
    (evil-define-key 'normal org-mode-map        (kbd "C-c c")   #'org-capture)
    (evil-define-key 'normal org-mode-map        (kbd "<SPC>op") #'org-previous-link)
    (evil-define-key 'normal org-mode-map        (kbd "<SPC>on") #'org-next-link)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "j")       #'org-agenda-next-line)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "k")       #'org-agenda-previous-line)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "h")       #'left-char)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "l")       #'right-char)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd " ")       #'org-agenda-cycle-show)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-c a")   #'org-agenda)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-c c")   #'org-capture)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-j")     #'evil-window-down)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-k")     #'evil-window-up)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-h")     #'evil-window-left)
    (evil-define-key 'emacs  org-agenda-mode-map (kbd "C-l")     #'evil-window-right)
    (evil-define-key 'insert org-mode-map        (kbd "M-.")     #'cp/org-surround-tilda)
    (evil-define-key 'insert org-mode-map        (kbd "M-v")     #'cp/org-surround-equal)
    (evil-define-key 'insert org-mode-map        (kbd "M-b")     #'cp/org-surround-star)
    (evil-define-key 'insert org-mode-map        (kbd "M-u")     #'cp/org-helm-complete-user-name-at-point)
    (advice-add 'org-next-link     :after #'cp/advice/org-next-link)
    (advice-add 'org-previous-link :after #'cp/advice/org-previous-link)
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
	 (add-hook
	  'write-contents-functions
	  (lambda ()
	    (save-excursion
	      (delete-trailing-whitespace)))))))
    (add-hook
     'org-src-mode-hook
     (lambda ()
       (setq electric-indent-mode nil)))))


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


;; dired
(use-package dired
  :defer t
  :config
  (progn
    (use-package dired-x)
    (put 'dired-find-alternate-file 'disabled nil)
    (evil-define-key 'normal dired-mode-map (kbd "n")   #'evil-search-next)
    (evil-define-key 'normal dired-mode-map (kbd "N")   #'evil-search-previous)
    (evil-define-key 'normal dired-mode-map (kbd "?")   #'evil-search-backward)
    (evil-define-key 'normal dired-mode-map (kbd "G")   #'evil-goto-line)
    (evil-define-key 'normal dired-mode-map (kbd "gg")  #'evil-goto-first-line)
    (evil-define-key 'normal dired-mode-map (kbd "M-k") #'dired-kill-subdir)
    (evil-define-key 'normal dired-mode-map (kbd "j")   #'dired-next-line)
    (evil-define-key 'normal dired-mode-map (kbd "k")   #'dired-previous-line)
    (evil-define-key 'normal dired-mode-map (kbd "h")   #'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l")   #'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map (kbd "J")   #'dired-next-subdir)
    (evil-define-key 'normal dired-mode-map (kbd "K")   #'dired-prev-subdir)
    (evil-define-key 'normal dired-mode-map (kbd "SPC") #'dired-display-file)
    (evil-define-key 'normal dired-mode-map (kbd "TAB") #'dired-hide-subdir)
    (evil-define-key 'normal dired-mode-map (kbd "o")   #'dired-find-file-other-window)
    (evil-define-key 'normal dired-mode-map (kbd "v")   #'dired-toggle-marks)
    (evil-define-key 'normal dired-mode-map (kbd "m")   #'dired-mark)
    (evil-define-key 'normal dired-mode-map (kbd "u")   #'dired-unmark)
    (evil-define-key 'normal dired-mode-map (kbd "U")   #'dired-unmark-all-marks)
    (evil-define-key 'normal dired-mode-map (kbd "c")   #'dired-create-directory)
    (evil-define-key 'normal dired-mode-map (kbd "q")   #'kill-this-buffer)
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))


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
       (define-key evil-normal-state-local-map (kbd "RET") #'cscope-select-entry-inplace)
       (define-key evil-normal-state-local-map (kbd "SPC") #'cscope-show-entry-other-window)
       (define-key evil-normal-state-local-map (kbd   "o") #'cscope-select-entry-other-window)
       (define-key evil-normal-state-local-map (kbd   "q") #'cscope-bury-buffer)
       (define-key evil-normal-state-local-map (kbd "M-n") #'cscope-history-forward-line)
       (define-key evil-normal-state-local-map (kbd "M-p") #'cscope-history-backward-line)
       (define-key evil-normal-state-local-map (kbd "M-k") #'cscope-history-kill-result)))))

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


;; helm-org-rifle
(use-package helm-org-rifle
  :defer t
  :config
  (progn
    (setq helm-org-rifle-show-path t)))


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
  :bind-keymap ("C-c p" . projectile-mode-map)
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


;; elisp-slime-nav
(use-package elisp-slime-nav
  :diminish (elisp-slime-nav-mode . "SN")
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  :config
  (progn
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "C-c ;") 'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "C-c &") 'pop-tag-mark)
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "C-c C-t") 'elisp-slime-nav-describe-elisp-thing-at-point)))


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


;; ace-jump
(use-package ace-jump
  :defer t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "<SPC>a") 'ace-jump-mode)))


;; lisp-mode
(use-package lisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (progn
                (setq indent-tabs-mode nil)
                (hs-minor-mode)
                (hs-hide-all)))))


;; hideshow
(use-package hideshow
  :defer t
  :config
  (progn
    (add-hook 'hs-minor-mode-hook
              (lambda ()
                (evil-local-set-key 'normal (kbd "TAB") #'hs-toggle-hiding)))))


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
    (add-hook 'sh-mode-hook
              (lambda ()
                (cp/sh-switch-to-indentation 8)
                (electric-indent-mode nil)))))


;; grep
(use-package grep
  :defer t
  :config
  (progn
    (setq grep-find-use-xargs 'gnu)
    (evil-define-key 'normal grep-mode-map (kbd "SPC") #'compilation-display-error)
    (evil-define-key 'normal grep-mode-map (kbd   "j") #'compilation-next-error)
    (evil-define-key 'normal grep-mode-map (kbd   "p") #'compilation-next-error)
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
  :config
  (progn
    (evil-define-key 'motion Man-mode-map (kbd "TAB") #'hs-toggle-hiding)
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


;; smartparens/evil-smartparens
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
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)))


;; highlight-parentheses
(use-package highlight-parentheses
  :defer t
  :config
  (progn
    (zenburn-with-color-variables
      (setq hl-paren-colors
            `(,zenburn-red-4
              ,zenburn-green
              ,zenburn-yellow-2
              ,zenburn-blue+1)))))


;; Random other things

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
    ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" default))))

; zenburn theme overrides
(zenburn-with-color-variables
  (custom-theme-set-faces
   `zenburn
   `(isearch                  ((t (:foreground ,zenburn-bg-05 :weight bold :background ,zenburn-orange))))
   `(lazy-highlight           ((t (:foreground ,zenburn-bg-05 :weight bold :background ,zenburn-orange))))
   `(diff-added               ((t (:foreground ,zenburn-green :weight bold))))
   `(diff-removed             ((t (:foreground ,zenburn-red))))
   `(flx-highlight-face       ((t (:foreground ,zenburn-red :weight normal))))
   `(linum                    ((t (:foreground ,zenburn-green+1 :background ,zenburn-bg))))
   '(dired-perm-write         ((t nil)))
   '(helm-buffer-directory    ((t (:foreground "color-247"))))
   '(helm-ff-dotted-directory ((t (:foreground "color-247"))))
   '(helm-match               ((t (:foreground "gold1" :weight normal))))))


; 2014-04-08: local emacs overrides
(let ((local "~/.emacs.local"))
  (when (file-exists-p local) (load-file local)))
