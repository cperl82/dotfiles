(use-package org
  :defer t
  :config
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 1)
  (setq org-startup-folded t)
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "DFER(r)" "WAIT(w)" "DPND(x)" "|" "DONE(d!)" "CNCL(c!)")))
  ;; CR-someday cperl: These should really be a part of whatever theme you're
  ;; loading, not just set unconditionally
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
  ;; CR-someday cperl: Consider setting this via .dir-locals.el as
  ;; sometimes you'll fire up an instance of emacs futzing with an org
  ;; file and it has nothing to do with your personal org files.
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
  (setq org-agenda-hide-tags-regexp (rx line-start (or "ATTACH") line-end))
  (add-hook 'org-agenda-finalize-hook #'cp/org-agenda-delete-empty-blocks)
  (setq org-tags-column -80)
  (setq org-log-into-drawer t)
  (setq org-refile-use-cache t)
  (setq org-catch-invisible-edits 'error)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-hide-leading-stars t)
  ;; CR cperl: Perhaps update this so it only does this if it
  ;; detects that `~/org' points at Dropbox
  (setq auto-revert-notify-exclude-dir-regexp
        (concat
         auto-revert-notify-exclude-dir-regexp
         "\\|"
         (rx line-start (0+ "/" (1+ anything)) "/" "org" "/")))
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
  (remove-hook 'org-mode-hook 'org-eldoc-load))

(use-package appt
  :after org
  :config
  (appt-activate t)
  (setq appt-message-warning-time 15)
  ; (setq appt-display-mode-line nil)
  ; (setq appt-display-interval appt-message-warning-time)
  ;; Add code here to automatically run `cp/org-agenda-to-appt'
  ;; at certain times, you need to run this to move any
  ;; scheduled or deadline tasks from org to appt for
  ;; notifications.
  (setq appt-disp-window-function #'cp/org-appt-disp-window)
  (setq appt-delete-window-function (lambda () t)))

(use-package org-attach
  :after org
  :config
  (setq org-attach-use-inheritance t)
  (setq org-attach-id-dir "~/org/data"))

(use-package org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org-habit
  :after org
  :config
  (progn
    (setq org-habit-today-glyph ?t)
    (setq org-habit-completed-glyph ?d)
    (setq org-habit-graph-column 69)
    (setq org-habit-preceding-days 21)
    (setq org-habit-following-days 3)))

(use-package org-tempo
  :after org)

(use-package ol-man
  :after org)

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
  ;; Add code here to encode the logic of displaying the appt
  ;; notification.  You have to test whether `min-to-app' is a list or
  ;; an atom, which can be done with `atom' You'll need to think about
  ;; how you want to override behavior for your various use cases
  ;;
  ;; If min-to-app is a list, then it means there are multiple
  ;; notifications and you should handle them all.
  ;;
  ;; Example:
  ;;
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
  (let ((lines
         (->>
          (process-lines "git" "status" "--porcelain")
          (-map #'s-trim))))
    (when (> (length lines) 0)
      (process-lines "git" "commit" "-a" "-m" "Automatic commit")
      t)))

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
