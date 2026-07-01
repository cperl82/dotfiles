;; -*- lexical-binding: t -*-
(add-to-list 'load-path (expand-file-name "init-lisp" user-emacs-directory))
(load "init-emacs")
(load "init-straight")
(load "init-general")
(load "init-utils")
(load "init-hippie-exp")
(load "init-uniquify")
(load "init-windsize")
(load "init-ibuffer-vc")
(load "init-which-key")
(load "init-dired")
(load "init-escreen")
(load "init-xcscope")
(load "init-smartparens")
(load "init-evil")
(load "init-embark")
(load "init-lisp")
(load "init-org")

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

(use-package counsel-projectile
  :defer t
  :after (projectile))


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


;; doom-modeline
(use-package doom-modeline
    :after (evil)
    :init
    (progn
      (setq doom-modeline-icon nil)
      (doom-modeline-mode 1)))


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
(load "init-local-overrides")
