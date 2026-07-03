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
(load "init-swiper-ivy-counsel")
(load "init-kdl")
(load "init-lisp")
(load "init-shell")
(load "init-rust")
(load "init-ocaml")
(load "init-org")
(load "init-projectile")

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
