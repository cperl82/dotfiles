(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror) (with-current-buffer
    (url-retrieve-synchronously
    "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))

(setq el-get-verbose t)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user-receipes")

(setq my-packages
      '(ack-and-a-half
        chumpy-windows
        color-theme
        color-theme-almost-monokai
        color-theme-ir-black
        color-theme-mac-classic
        color-theme-solarized
        color-theme-tomorrow
        color-theme-zenburn
        xoria256-emacs
        escreen
        evil
        undo-tree
        evil-leader
        tuareg-mode
        org-mode
        buffer-move
        project-explorer
        xcscope
        xcscope+))

(el-get 'sync my-packages)

; 2014-03-27: Turn off the menu bar
(menu-bar-mode -1)

; 2014-03-27: Always show matching parents
(setq show-paren-delay 0)
(show-paren-mode)

; 2014-03-27: Do not want backup files
(setq make-backup-files nil)

; 2014-03-27: Evil
(setq-default evil-symbol-word-search t)
(require 'evil)
(evil-mode 1)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'help-mode    'normal)

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
  "o" 'delete-other-windows
  "x" 'delete-window
  "e" 'escreen-get-active-screen-numbers-with-emphasis)

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
  (let* ((search-string (buffer-substring beg end))
	 (quoted-string (regexp-quote search-string)))
    (setq isearch-forward forward)
    (evil-search quoted-string forward t)))

(evil-define-operator cp-evil-search-forward (beg end type)
  (cp-evil-search beg end t))

(evil-define-operator cp-evil-search-backward (beg end type)
  (cp-evil-search beg end nil))

(define-key evil-visual-state-map "/" `cp-evil-search-forward)
(define-key evil-visual-state-map "?" `cp-evil-search-backward)

; 2014-03-27: ack-and-a-half: https://github.com/jhelwig/ack-and-a-half
(require 'ack-and-a-half)

; 2014-03-29: org-mode
(require 'org)

; 2014-03-29: ido
(require 'ido)
(ido-mode t)

; 2014-03-30: tuareg mode
(require 'tuareg)

;;; escreen
(require 'escreen)
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (concat "(*" (number-to-string s) ")")
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

(defun dim:escreen-goto-last-screen ()
  (interactive)
  (escreen-goto-last-screen)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-prev-screen (&optional n)
  (interactive "p")
  (escreen-goto-prev-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-next-screen (&optional n)
  (interactive "p")
  (escreen-goto-next-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

(global-set-key '[M-S-right] (quote dim:escreen-goto-next-screen))
(global-set-key '[M-S-left] (quote dim:escreen-goto-prev-screen))

(global-set-key (kbd "M-[") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'dim:escreen-goto-next-screen)

; 2014-04-03: Org mode customizations
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-log-done 'time)
(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "SPC") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

(evil-define-key 'insert org-mode-map (kbd "C-c ,")
  '(lambda () (interactive) (org-time-stamp-inactive t)))

(evil-define-key 'insert org-mode-map (kbd "C-c .")
  '(lambda () (interactive) (org-time-stamp-inactive)))

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-c C-h") 'help)
(global-set-key (kbd "C-c h")   'help)

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

; 2014-04-06: cscope related
; (evil-define-key 'normal cscope-minor-mode-keymap (kbd "<S-return>") 'cscope-select-entry-inplace)

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
 '(custom-safe-themes (quote ("4031c1ea0bb235b75a048bd92f3bf3aa984c9f7cc5b408f00f62ed99a6eecc09" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
