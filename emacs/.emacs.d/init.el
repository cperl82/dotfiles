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
(load "init-kdl")
(load "init-lisp")
(load "init-shell")
(load "init-rust")
(load "init-ocaml")
(load "init-org")
(load "init-projectile")

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
  :after ivy
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
  :after projectile)


;; kdl-mode
(use-package kdl-mode
  :defer t)


;; doom-modeline
(use-package doom-modeline
    :after evil
    :init
    (progn
      (setq doom-modeline-icon nil)
      (doom-modeline-mode 1)))


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
