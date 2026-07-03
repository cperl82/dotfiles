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
(load "init-theme-zenburn")
(enable-theme 'zenburn)

(load "init-local-overrides")
