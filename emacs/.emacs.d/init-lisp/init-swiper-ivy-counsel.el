(use-package swiper
  :straight t
  :demand t)

(use-package ivy
  :straight t
  :demand t
  :custom
  (ivy-height 10)
  (ivy-initial-inputs-alist nil)
  ;; 2017-05-06: `ivy--regex-ignore-order' doesn't seem to work well with swiper as the
  ;; buffer highlights don't seem to be applied correctly.  However, its useful when
  ;; using `counsel-M-x', so enable it for that (e.g. typing `dired find' to find
  ;; `find-dired').
  (ivy-re-builders-alist
   '((counsel-M-x . ivy--regex-ignore-order)
     (t . ivy--regex-plus)))
  (ivy-count-format "%d/%d ")
  :config
  (setf (cdr (assoc t ivy-format-functions-alist)) #'ivy-format-function-arrow)
  (ivy-mode))

(use-package counsel
  :straight t
  :demand t
  :custom
  ;; Use rg instead of grep because it has the nice smart case feature
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (counsel-grep-use-swiper-p #'cp/counsel-grep-use-swiper-p)
  :config
  (counsel-mode))

(use-package ivy-rich
  :straight t
  :defer t
  :after ivy
  :custom
  (ivy-rich-path-style 'relative)
  :config
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
  (ivy-rich-mode))

(use-package counsel-projectile
  :straight t
  :defer t
  :after projectile)

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
