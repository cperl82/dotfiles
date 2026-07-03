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
       diminish
       elisp-slime-nav
       embark
       flycheck
       go-mode
       haskell-mode
       highlight-parentheses
       ibuffer-projectile
       json-mode
       lsp-mode
       lua-mode
       kdl-mode
       magit
       markdown-mode
       nasm-mode
       nerd-icons
       org-ql
       origami
       projectile
       smex
       systemd-mode
       systemtap-mode
       tuareg-mode
       unfill
       vagrant-tramp
       wgrep
       windsize
       zenburn-theme))
  (straight-use-package package))
