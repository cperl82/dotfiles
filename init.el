(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror) (with-current-buffer
    (url-retrieve-synchronously
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path "~/.emacs.d/user-receipes")

(el-get
 'sync
 '(color-theme-zenburn
   xoria256-emacs
   undo-tree
   evil
   evil-leader
   tuareg-mode
   org-mode
   xcscope
   flx
   ido-vertical-mode
   rainbow-mode
   helm
   helm-cmd-t
   systemtap-mode
   haskell-mode
   lua-mode))

; 2014-04-26: Loading other stuff
(add-to-list 'load-path "~/.emacs.d/lisp")

(defun format-default-dir-for-mode-line (d max-length)
  (let* ((reduced
	  (if (string-match (format "^%s" (getenv "HOME")) d)
	      (replace-match "~" t t d)
	    d))
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
(setq-default column-number-mode t)
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		(:eval (format-default-dir-for-mode-line default-directory 40))
		"   "
		mode-line-position
		evil-mode-line-tag
		(vc-mode vc-mode)
		"  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

; 2014-08-12
; I prefer the window to be split horizontally unless I explicitly split it
; veritcally
(setq split-height-threshold nil)

; 2014-04-24: Duplicate buffer names
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

; 2014-03-27: Turn off the menu bar
(menu-bar-mode -1)

; 2014-03-27: Always show matching parents
(setq-default show-paren-delay 0)
(show-paren-mode)

; 2014-03-27: Do not want backup files
(setq make-backup-files nil)

; 2014-03-27: Evil
(setq-default evil-symbol-word-search t)
(setq-default evil-flash-delay 5)
(require 'evil)
(evil-mode 1)

; 2014-03-27: Evil Leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'find-file
  "b" 'ibuffer
  "s" 'split-window-vertically
  "v" 'split-window-horizontally
  "k" 'kill-buffer
  "K" 'kill-buffer-and-window
  "o" 'delete-other-windows
  "x" 'delete-window
  "e" 'escreen-get-active-screen-names-with-emphasis
  "h" 'cp-evil-highlight-symbol
  "E" '(lambda () (interactive) (message (buffer-file-name))))

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
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map  [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)

; 2014-03-28: Functions to support selecting something in Visual mode
; and then automatically start searching for it by pressing "/" or "?"
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

; 2014-03-29: org-mode
(require 'org)

; 2014-04-27: Initial function for copying gmail links to clipboard
(org-add-link-type "gmail" 'org-gmail-copy-to-clipboard)

(defun org-gmail-copy-to-clipboard (url)
  (let* ((process-connection-type nil)
	 (proc (start-process "pbcopy" nil "pbcopy")))
    (process-send-string proc (format "https://mail.google.com/u/0/#all/%s" url))
    (process-send-eof proc)))

; 2014-12-07: ido / flx
(require 'ido)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

; 2014-12-07 ido-vertical-mode
(require 'ido-vertical-mode)
(setq-default ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode 1)

; 2014-03-30: tuareg mode
(require 'tuareg)

; 2014-05-14: This isn't perfect, but it works well enough in practice
; that I'm including it.
(defun cperl-selective-display-forward-sexp-fun (arg)
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
       "\\<type\\>\\(\\s-+\\S-+\\)+?\\s-+="
       "\\<TEST_MODULE\\>\\s-+\\S-+\\s-+=\\s-+\\<struct\\>"
       "\\<TEST_UNIT\\>\\s-+="
       )
     "\\|")
   nil nil  cperl-selective-display-forward-sexp-fun))

;;; escreen
(require 'escreen)

(defun escreen-swap-screen (other-screen-number)
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

(defun escreen-move-screen (direction)
  (let ((other-screen-number
	 (cond ((eq direction 'left)  (1- escreen-current-screen-number))
	       ((eq direction 'right) (1+ escreen-current-screen-number)))))
    (cond ((and
	    (>= other-screen-number 0)
	    (<= other-screen-number escreen-highest-screen-number-used))
	   (escreen-swap-screen other-screen-number))
	  ; These are the cases where we're moving right off the right
	  ; end or left off the left end
	  ; TODO: some of the below can probably be factored out
	  ((< other-screen-number 0)
	   (let ((n 1)
		 (end escreen-highest-screen-number-used))
	    (while (<= n end)
	      (escreen-swap-screen n)
	      (setq n (1+ n)))))
	  ((> other-screen-number escreen-highest-screen-number-used)
	   (let ((n (1- escreen-highest-screen-number-used)))
	     (while (>= n 0)
	       (escreen-swap-screen n)
	       (setq n (1- n)))))))
  (escreen-get-active-screen-names-with-emphasis))

(defun escreen-move-screen-left ()
  (interactive)
  (escreen-move-screen 'left))

(defun escreen-move-screen-right ()
  (interactive)
  (escreen-move-screen 'right))

(defun escreen-rename-screen (&optional name number suppress-message)
  (interactive "sNew screen name: ")
  (let ((screen-data (escreen-configuration-escreen (or number escreen-current-screen-number)))
	(new-name (cond ((equal name "") nil)
			((stringp name) name)
			(t "default"))))
    (setcar (cdr screen-data) new-name)
    (when (not suppress-message)
      (escreen-get-active-screen-names-with-emphasis))))

(defun escreen-get-active-screen-names-with-emphasis()
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

(defadvice escreen-goto-screen (after cp/escreen-goto-screen first (n &optional dont-update-current) activate)
  (escreen-get-active-screen-names-with-emphasis))

(defadvice escreen-kill-screen (after cp/escreen-kill-screen first (&optional n) activate)
  (escreen-get-active-screen-names-with-emphasis))

(defadvice escreen-create-screen (after cp/escreen-create-screen first (&optional n) activate)
  (escreen-rename-screen)
  (escreen-get-active-screen-names-with-emphasis))

(defadvice escreen-install (after cp/escreen-install activate)
  (escreen-rename-screen nil nil t))

(escreen-install)

(global-set-key         (kbd "C-\\") 'escreen-prefix)
(define-key escreen-map (kbd "r")    'escreen-rename-screen)
(define-key escreen-map (kbd "C-\\") 'escreen-goto-last-screen)
(define-key escreen-map (kbd "M-[")  'escreen-goto-prev-screen)
(define-key escreen-map (kbd "M-]")  'escreen-goto-next-screen)

; 2014-04-24: hide show related
; 2014-04-30: I'm not sure why the hook works but the `evil-define-key' doesn't (well, I
; mean it sort of works in that if I enter insert mode and then exit back into normal mode
; the keybinding will be there, but I want it to just be there right away).
(add-hook 'hs-minor-mode-hook
	  (lambda ()
            (cond ((eq major-mode 'Man-mode)
                   (define-key evil-motion-state-local-map (kbd "TAB") 'hs-toggle-hiding))
                  (t
                   (define-key evil-normal-state-local-map (kbd "TAB") 'hs-toggle-hiding)))))

; 2014-04-13: Custom keys for dired
(evil-define-key 'normal dired-mode-map (kbd "TAB")  'dired-hide-subdir)
(evil-define-key 'normal dired-mode-map (kbd "n")    'evil-search-next)
(evil-define-key 'normal dired-mode-map (kbd "N")    'evil-search-previous)
(evil-define-key 'normal dired-mode-map (kbd "?")    'evil-search-backward)
(evil-define-key 'normal dired-mode-map (kbd "G")    'evil-goto-line)
(evil-define-key 'normal dired-mode-map (kbd "gg")   'evil-goto-first-line)
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    (require 'dired-x)
	    (dired-omit-mode 1)))

; 2014-04-03: Org mode customizations
(add-hook 'org-mode-hook
          (lambda ()
            (progn
              'auto-fill-mode
              (setq fill-column 90))))
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-files '("~/org"))
(setq org-capture-templates
      '(("g" "Todo with Gmail Id" entry (file "~/org/capture.org") "* TODO  %?\n  [[gmail:%^{gmail id}][%\\1]]")
	("t" "Todo" entry (file "~/org/capture.org") "* TODO  %?\n")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "FWUP(f)" "DFER(r)" "|" "DONE(d)")))
(setq org-link-abbrev-alist
       '(("gmail"  . "https://mail.google.com/mail/u/0/#all/%s")))
(setq org-agenda-custom-commands
       `(("u" "Unscheduled tasks" tags-todo "-DEADLINE={.+}&-SCHEDULED={.+}")))
(setq org-refile-use-outline-path 'file)
(setq org-hide-block-startup t)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-catch-invisible-edits 'error)
(setq org-ctrl-k-protect-subtree t)
(evil-define-key 'normal org-mode-map (kbd "TAB")   'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "M-h")   'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-l")   'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-k")   'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j")   'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-H")   'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-L")   'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-K")   'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J")   'org-shiftmetadown)
(evil-define-key 'normal org-mode-map (kbd "C-c a") 'org-agenda)
(evil-define-key 'normal org-mode-map (kbd "C-c c") 'org-capture)
(evil-define-key 'emacs org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
(evil-define-key 'emacs org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
(evil-define-key 'emacs org-agenda-mode-map (kbd "h") 'left-char)
(evil-define-key 'emacs org-agenda-mode-map (kbd "l") 'right-char)

; Use different keys to access help
(global-set-key (kbd "C-x h") 'help-command)

(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-motion-state-map "\C-j" 'evil-window-down)
(define-key evil-motion-state-map "\C-k" 'evil-window-up)
(define-key evil-motion-state-map "\C-h" 'evil-window-left)
(define-key evil-motion-state-map "\C-l" 'evil-window-right)

(require 'buffer-move)
(define-key evil-normal-state-map (kbd "C-M-j") 'buf-move-down)
(define-key evil-normal-state-map (kbd "C-M-k") 'buf-move-up)
(define-key evil-normal-state-map (kbd "C-M-h") 'buf-move-left)
(define-key evil-normal-state-map (kbd "C-M-l") 'buf-move-right)
(define-key evil-motion-state-map (kbd "C-M-j") 'buf-move-down)
(define-key evil-motion-state-map (kbd "C-M-k") 'buf-move-up)
(define-key evil-motion-state-map (kbd "C-M-h") 'buf-move-left)
(define-key evil-motion-state-map (kbd "C-M-l") 'buf-move-right)

; 2014-04-29: man related
(require 'man)
(defun cperl-man-forward-sexp-fun (arg)
    (let ((p (point)))
      (forward-line 1)
      (re-search-forward (concat
			  Man-heading-regexp
			  "\\|"
			  "\\'"
			  "\\|"
			  "^[^[:space:]]"))
      (beginning-of-line)
      (forward-line -1)
      (if (equal p (point)) (end-of-line))))

(add-to-list
 'hs-special-modes-alist
 `(Man-mode
   ,Man-heading-regexp
   nil
   nil
   cperl-man-forward-sexp-fun))

(add-hook 'Man-mode-hook
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
	     nil     ; Copied from /usr/share/vim/vim74/syntax/man.vim
	     `((,Man-heading-regexp          . font-lock-comment-face)
	       ("^\\s-*[+-][a-zA-Z0-9]\\S-*" . font-lock-function-name-face)
	       ("^\\s-*--[a-zA-Z0-9-]\\S-*"  . font-lock-function-name-face))
	     'set)
            (font-lock-mode 1)))

; 2014-05-07: function to revert all buffers
(defun revert-buffer-all ()
  "Revert all buffers.  This reverts buffers that are visiting a file, kills
buffers whose visited file has disappeared and refreshes dired buffers."
  (interactive)
  (if (yes-or-no-p "Revert ALL buffers? ")
      (save-excursion
	(dolist (b (buffer-list))
	  (set-buffer b)
	  (cond
	   (buffer-file-name
	    (if (file-exists-p buffer-file-name)
		(revert-buffer t t t)
	      (kill-buffer b)))
	   ((eq major-mode 'dired-mode) (revert-buffer t t t)))))))
			  
; 2014-04-06: cscope related
(setq-default cscope-option-use-inverted-index t)
(setq-default cscope-close-window-after-select t) 
(setq-default cscope-edit-single-match nil)
(setq-default cscope-option-kernel-mode t)
(add-hook 'cscope-list-entry-hook
	  (lambda ()
	    (setq-local face-remapping-alist
			'((cscope-separator-face   font-lock-string-face)
			  (cscope-line-number-face font-lock-string-face)
			  (cscope-file-face        font-lock-doc-face)
			  (cscope-function-face    font-lock-function-name-face)))
	    (define-key evil-normal-state-local-map (kbd "RET") 'cscope-select-entry-inplace)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'cscope-show-entry-other-window)
	    (define-key evil-normal-state-local-map (kbd   "o") 'cscope-select-entry-other-window)
	    (define-key evil-normal-state-local-map (kbd   "q") 'cscope-bury-buffer)
	    (define-key evil-normal-state-local-map (kbd "M-n") 'cscope-history-forward-line)
	    (define-key evil-normal-state-local-map (kbd "M-p") 'cscope-history-backward-line)
	    (define-key evil-normal-state-local-map (kbd "M-k") 'cscope-history-kill-result)))

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
    ("f37d09076188b2e8d2a6847931deec17f640853aedd8ea4ef3ac57db01335008" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "e5067916703453e96ff3f9c1ce1f436aa0716f9207e2c53266a9d77d2b575669" "c06241fdef16c336d0cc3a96cf5de5725fad678e9313513e586f0a243289ada5" "de892595c7a5462962a8f20e48f95f992e9eba367e920ac2add84bb95bd65366" "4031c1ea0bb235b75a048bd92f3bf3aa984c9f7cc5b408f00f62ed99a6eecc09" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-perm-write ((t nil)))
 '(flx-highlight-face ((t (:foreground "#CC9393" :weight normal)))))

; 2014-12-06 override zenburn's default isearch highlighting
(custom-theme-set-faces
 `zenburn
 `(isearch ((t (:foreground ,"#383838" :weight bold :background ,"#DFAF8F"))))
 `(lazy-highlight ((t (:foreground ,"#383838" :weight bold :background ,"#DFAF8F")))))

; 2014-12-07 Trying to make sh-mode indentation better
; Copied from http://keramida.wordpress.com/2008/08/08/tweaking-shell-script-indentation-in-gnu-emacs
(defun cperl-setup-sh-mode ()
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
(add-hook 'sh-mode-hook 'cperl-setup-sh-mode)

; 2014-12-10 Starting to play with helm.
(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action)

; 2014-12-12 helm-cmd-t: larger candidate list and cache invalidate
(require 'helm-cmd-t)
(defun js-helm-cmd-t-sources ()
  (let ((root-data (helm-cmd-t-root-data)))
    (if root-data
	(helm-cmd-t-get-create-source root-data)
      nil)))

(defun js-helm-cmd-t (arg)
  (interactive "p")
  (if (eq arg 4)
      (call-interactively 'helm-cmd-t-repos)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources (js-helm-cmd-t-sources)
            :candidate-number-limit (if (= arg 1) 200 arg)
            :buffer "*helm-cmd-t:*"))))

(defun helm-cmd-t-invalidate-cache (data)
  (setf (cdr (assq 'cached-p data)) nil))

(defadvice helm-cmd-t-get-create-source
    (before cp/helm-cmd-t-get-create-source first (repo-root-data &optional skeleton) activate)
  (let* ((repo-root (cdr repo-root-data))
         (repo-type (car repo-root-data))
         (source-buffer-name (helm-cmd-t-get-source-buffer-name repo-root))
         (candidate-buffer (get-buffer-create source-buffer-name))
         (data (buffer-local-value 'helm-cmd-t-data candidate-buffer)))
    (when (cdr (assq 'cached-p data))
      ; TODO: Support git, bzr, etc with a lookup table for what to
      ; check to see if the working directory has changed
      (let* ((work-directory-proxy (concat repo-root ".hg/dirstate"))
	     (cache-time-stamp (cdr (assq 'time-stamp data)))
	     (file-time-stamp (float-time (nth 5 (file-attributes work-directory-proxy)))))
	(if (file-exists-p work-directory-proxy)
	    (when (> file-time-stamp cache-time-stamp)
	      (progn
		(message "Cache is older than mtime of %s, invalidating" work-directory-proxy)
		(helm-cmd-t-invalidate-cache data)))
	  (when (> (- (float-time) 1800) cache-time-stamp)
	    (progn
	      (message "Cache is older than 30m, invalidating")
	      (helm-cmd-t-invalidate-cache data))))))))

; 2015-02-16: grep related stuff
(setq grep-find-use-xargs 'gnu)
(add-to-list 'grep-files-aliases '("ml"  . "*.ml *.mli"))
(add-to-list 'grep-files-aliases '("mlc" . "*.ml *.mli *.c *.h"))

; 2014-04-08: local emacs overrides
(let ((local "~/.emacs.local"))
  (when (file-exists-p local) (load-file local)))
