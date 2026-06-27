;; -*- lexical-binding: t -*-
(add-to-list 'load-path (expand-file-name "init-lisp" user-emacs-directory))
(require 'init-emacs)
(require 'init-straight)
(require 'init-general)
(require 'init-utils)
(require 'init-hippie-exp)
(require 'init-uniquify)
(require 'init-windsize)
(require 'init-ibuffer-vc)
(require 'init-which-key)
(require 'init-dired)
(require 'init-escreen)
(require 'init-xcscope)
(require 'init-evil)
(require 'init-embark)
(require 'init-lisp)

;; swiper / ivy / ivy-rich / counsel
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

(use-package swiper
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
  :init
  (progn
    ;; Use rg instead of grep because it has the nice smart case feature
    (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")
    (setq counsel-grep-use-swiper-p #'cp/counsel-grep-use-swiper-p)
    (counsel-mode 1)))


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


;; sh-script
(defun cp/sh-mode-hook-setup ()
  (sh-set-shell "bash")
  (flycheck-mode)
  (flycheck-select-checker 'sh-shellcheck)
  (hs-minor-mode)
  (hs-hide-all))

(use-package sh-script
  :defer t
  :config
  (progn
    (add-hook 'sh-mode-hook #'cp/sh-mode-hook-setup)))


;; kdl-mode
(use-package kdl-mode
  :defer t)


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
      (doom-modeline-mode 1)))


;; org
(defun cp/org-generate-short-id ()
  "Generate a short mostly unique identifier"
  (interactive)
  (let ((str
        (cl-loop with result = ""
                 with alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                 repeat 6
                 concat
                 (let ((idx (random (length alphabet))))
                   (substring alphabet idx (1+ idx)))
                 into result
                 finally return result)))
    (if (called-interactively-p 'interactive)
        (message str)
      str)))

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
         (rotated (-rotate n-finished-states (-concat (reverse not-finished) (reverse finished)))))
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
  ;; there may be a better way to do this, but for now its refolds
  ;; things the way I want after sorting
  (funcall (general-simulate-key "TAB TAB")))

(defun cp/org-agenda-tagged-by-context (search-string)
  ;; CR-someday cperl: Have to figure out how to make C-u r in the
  ;; buffer work correctly.  Right now it regenerated the buffer with
  ;; a different tag, but winds up leaving it named incorrectly.  I
  ;; was thinking one way around it might be to substitute in the
  ;; proper to call to this function for `org-agenda-redo-command' and
  ;; the `redo-cmd' text property, but I could quite get it working.
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


;; 2025-02-22 cperl: Snarfed from:
;; https://www.reddit.com/r/emacs/comments/jjrk2o/comment/gaeh3st/
(defun cp/org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2 non-empty lines in
  the block (excluding the line with `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(defun cp/org-save-all-org-buffers-and-commit ()
  (interactive)
  (org-save-all-org-buffers)
  (let* ((lines
          (->>
           (process-lines "git" "status" "--porcelain")
           (-map #'s-trim))))
    (if (> (length lines) 0)
        (progn
          (process-lines "git" "commit" "-a" "-m" "Automatic commit")
          t))))

(defun cp/advice/org-archive--compute-location (location)
  "A function for use as :filter-args advice on `org-archive--compute-location'.

This function escapes any %s's in LOCATION by turning %s into %%s
and then runs LOCATION through `format-time-string'. This allows
having archive locations like:

  archive/%Y/%s_archive::* Tasks

via ARCHIVE properties.

Now when calling `org-archive-subtree-default' the headline will be
archived to the current year's subdirectory. If the subdirectory doesn't
exist, you'll be prompted to create it.

This means we'll be able to keep archive files to a reasonable size
rather than growing indefinitely.

Note that because we're being called via advice as :filter-args, we
receive a list of `org-archive--compute-locations's arguments and have
to return a list"
  (list (format-time-string (replace-regexp-in-string "%s" "%%s" (car location)))))

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
   "o L"     #'org-shiftmetaright
   "o c"     #'cp/org-save-all-org-buffers-and-commit)
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
   "M-,"     #'cp/org-surround-equal)
  (:keymaps '(org-agenda-mode-map)
   :states  '(emacs)
   "j"       #'org-agenda-next-line
   "k"       #'org-agenda-previous-line
   "h"       #'left-char
   "l"       #'right-char
   "G"       #'evil-goto-line
   "gg"      #'evil-goto-first-line
   "C-c a"   #'org-agenda
   "C-c c"   #'org-capture
   "s"       #'cp/org-save-all-org-buffers-and-commit)
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
    (use-package org-attach
      :config
      (progn
        (setq org-attach-use-inheritance t)
        (setq org-attach-id-dir "~/org/data")))
    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))
    (use-package org-habit
      :config
      (progn
        (setq org-habit-today-glyph ?t)
        (setq org-habit-completed-glyph ?d)
        (setq org-habit-graph-column 69)
        (setq org-habit-preceding-days 21)
        (setq org-habit-following-days 3)))
    (use-package ol-man)
    (use-package org-tempo)
    (setq org-adapt-indentation nil)
    (setq org-startup-indented t)
    (setq org-indent-indentation-per-level 1)
    (setq org-startup-folded t)
    (setq org-todo-keywords
          '((sequence "NEXT(n)" "DFER(r)" "WAIT(w)" "DPND(x)" "|" "DONE(d!)" "CNCL(c!)")))
    (setq org-todo-keyword-faces
          '(("DFER" . "#B06060")
            ("WAIT" . "#8C5353")
            ("DPND" . "#767676")
            ("CNCL" . "#FFFFFF")
            ("DONE" . "#FFFFFF")))
    (setq org-capture-templates
          `(("n" "Next Action" entry (file "~/org/capture.org")
             ,(string-join '("* NEXT %?" ":PROPERTIES:" ":CAPTURED: %U" ":END:") "\n")
             :empty-lines 1)))
    (setq org-link-abbrev-alist
          '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-hide-block-startup t)
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-files (file-expand-wildcards "~/org/*.org" t))
    (setq org-agenda-block-separator ?-)
    (setq org-agenda-format-date "%a %Y-%m-%d")
    (setq org-agenda-sticky t)
    (setq org-agenda-prefix-format
          '((agenda . "  %7c %6s %5t ")
            (todo   . "  %7c ")
            (tags   . "  %7c ")
            (search . "  %7c ")))
    (setq org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            ""
            "----------------"))
    (setq org-agenda-breadcrumbs-separator ">")
    (setq org-agenda-scheduled-leaders '("s" "%dd s"))
    (setq org-agenda-deadline-leaders '("d" "-%dd d" "%dd d"))
    (setq org-agenda-format-date
          (lambda (date)
            (concat "\n" (format-time-string
                          "%a %Y-%m-%d:"
                          (org-time-from-absolute date)))))
    (setq org-agenda-remove-tags t)
    (setq org-agenda-show-future-repeats nil)
    (setq org-agenda-hide-tags-regexp (format "^%s$" (regexp-opt '("ATTACH"))))
    (add-hook 'org-agenda-finalize-hook #'cp/org-agenda-delete-empty-blocks)
    (setq org-tags-column -80)
    (setq org-log-into-drawer t)
    (setq org-refile-use-cache t)
    (setq org-catch-invisible-edits 'error)
    (setq org-ctrl-k-protect-subtree t)
    (setq org-cycle-include-plain-lists 'integrate)
    (setq org-hide-leading-stars t)
    (setq org-link-make-description-function  #'cp/org-link-auto-desc-from-abbrev-tags)
    ;; CR cperl: Perhaps update this so it only does this if it
    ;; detects that `~/org' points at Dropbox
    (setq auto-revert-notify-exclude-dir-regexp
          (concat
           auto-revert-notify-exclude-dir-regexp
           "\\|"
           (rx (: bol (0+ "/" (1+ anything)) "/" "org" "/"))))
    ;; Ensure certain files are opened in certain apps
    (add-to-list 'org-file-apps '("\\.png\\'" . "eog %s"))
    (add-to-list 'org-file-apps '("\\.jpe?g\\'" . "eog %s"))
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
    (run-with-idle-timer 30 t
                         (lambda ()
                           (let ((inhibit-message t)) (org-save-all-org-buffers))))
    (advice-add  'org-next-link     :after #'cp/advice/org-next-link)
    (advice-add  'org-previous-link :after #'cp/advice/org-previous-link)
    (advice-add  'org-archive--compute-location :filter-args #'cp/advice/org-archive--compute-location)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell  . true)
       (python . true)
       (awk    . true)
       (sed    . true)
       (R      . true)
       (calc   . true)))
    (add-hook
     'org-mode-hook
     (lambda ()
       (progn
         (define-and-bind-text-object "~" "\\~" "\\~")
         (define-and-bind-text-object "*" "\\*" "\\*")
         (define-and-bind-text-object "=" "\\=" "\\=")
         (visual-line-mode)
         (visual-fill-column-mode)
         (setq-local visual-fill-column-width 95)
         (setq-local indent-tabs-mode nil)
         (add-hook 'write-contents-functions
                   (lambda () (save-excursion (delete-trailing-whitespace)))))))
    (add-hook 'org-src-mode-hook
              (lambda ()
                (setq-local electric-indent-mode nil)
                (define-key org-src-mode-map
                            (kbd "C-c @")
                            #'org-src-do-key-sequence-at-code-block)
                (define-key org-src-mode-map
                            [remap evil-write]
                            #'org-edit-src-save)))
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
  :after (projectile))


;; rust / rustic
(use-package rust-mode
  :defer t)

(use-package rustic-mode
  :defer t
  :after (rust-mode)
  :config
  (progn
    (add-hook
     'rustic-mode-hook
     (lambda ()
       (hs-minor-mode)
       (hs-hide-all)))))


;; Themes
;; zenburn
(use-package zenburn-theme
  :config
  (progn
    (defun cp/zenburn-theme-customize (theme)
      (when (eq theme 'zenburn)
        (set-face-attribute 'mode-line nil :box nil)
        (set-face-attribute 'mode-line-inactive nil :box nil)
        (zenburn-with-color-variables
          (custom-theme-set-faces
           'zenburn
           `(diff-added
             ((t (:foreground ,zenburn-green :weight bold))))
           `(diff-removed
             ((t (:foreground ,zenburn-red))))
           `(linum
             ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg))))
           '(dired-perm-write
             ((t nil)))
           ;; 2026-06-11 cperl: This is a workaround for zenburn
           ;; setting the default face for doom-modeline and causing
           ;; the inactive mode-line to not "fade" properly
           '(doom-modeline
             ((t ())))
           '(hl-line
             ((t (:background "#4F4F4F"))))
           `(isearch
             ((t (:inherit nil :foreground "white" :background ,zenburn-red-2))))
           `(lazy-highlight
             ((t (:inherit nil :foreground "white" :background ,zenburn-blue-1))))
           `(match
             ((t (:inherit nil :foreground "white" :background ,zenburn-yellow-2))))
           `(info-node
             ((t (:foreground ,zenburn-red-3))))
           '(ivy-cursor
             ((t (:foreground "#000000" :background "#d6d6d6"))))
           `(ivy-current-match
             ((t (:underline nil))))
           '(ivy-minibuffer-match-face-1
             ((t (:underline nil))))
           `(ivy-minibuffer-match-face-2
             ((t (:foreground ,zenburn-red-2))))
           `(ivy-minibuffer-match-face-3
             ((t (:foreground ,zenburn-blue-1))))
           `(ivy-minibuffer-match-face-4
             ((t (:foreground ,zenburn-yellow-2))))
           `(ivy-virtual
             ((t (:inherit font-lock-type-face))))
           '(swiper-line-face
             ((t (:underline nil :background "#4F4F4F"))))
           '(swiper-match-face-1
             ((t (:underline nil))))
           `(swiper-match-face-2
             ((t (:inherit nil :foreground "white" :background ,zenburn-red-2))))
           `(swiper-match-face-3
             ((t (:inherit nil :foreground "white" :background ,zenburn-blue-1))))
           `(swiper-match-face-4
             ((t (:inherit nil :foreground "white" :background ,zenburn-yellow-2))))))))
    (add-hook 'enable-theme-functions #'cp/zenburn-theme-customize))
  ;; 2026-06-11 cperl: Something is subtly wrong here, as `load-theme'
  ;; also enables the theme, but if we don't call `enable-theme' to
  ;; enable it again the evil state indicator won't be the correct
  ;; color (e.g. "N" will be green, not cyan, etc)
  (load-theme 'zenburn :no-confirm)
  (enable-theme 'zenburn)
  )
(require 'init-local-overrides)
