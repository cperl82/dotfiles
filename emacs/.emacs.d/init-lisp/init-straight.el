(setq straight-cache-autoloads t
        straight-check-for-modifications nil)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(dolist
    (package
     '(annalist
       company-mode
       compat
       counsel-projectile
       diminish
       elisp-slime-nav
       embark
       evil-smartparens
       flycheck
       go-mode
       haskell-mode
       highlight-parentheses
       ibuffer-vc
       ibuffer-projectile
       ivy-rich
       json-mode
       lsp-mode
       lua-mode
       kdl-mode
       magit
       markdown-mode
       nasm-mode
       nerd-icons
       org
       org-super-agenda
       org-ql
       origami
       projectile
       rust-mode
       rustic
       smartparens
       smex
       doom-modeline
       swiper
       systemd-mode
       systemtap-mode
       tuareg-mode
       unfill
       vagrant-tramp
       visual-fill-column
       wgrep
       windsize
       xcscope
       zenburn-theme))
  (straight-use-package package))

(provide 'init-straight)
