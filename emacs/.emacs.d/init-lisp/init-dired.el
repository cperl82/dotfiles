(use-package dired
  :defer t
  :hook
  ((dired-mode . dired-omit-mode))
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-dwim-target t)
  :bind
  (:map dired-mode-map
   ("h" . #'dired-up-directory)
   ("l" . #'dired-find-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :defer t
  :after dired)

(defun cp/dired-tab-dwim ()
  (interactive)
  (if (dired-get-subdir)
      (dired-hide-subdir 1)
    (ignore-errors
      (dired-find-file-other-window))))

(defun cp/dired-toggle-hiding-dotfiles ()
  (interactive)
    (let ((regex (rx bos ?. (1+ any) eos))
          (elements (s-split "\\\\|" dired-omit-files)))
      (if (-contains? elements regex)
          (setq dired-omit-files (s-join "\\|" (-remove-item regex elements)))
        (setq dired-omit-files (s-join "\\|" (-insert-at 0 regex elements))))
      (revert-buffer)
      (message "dired-omit-files is now: %S" dired-omit-files)))

(defun cp/dired-smart-async-shell-command (command &optional output-buffer error-buffer)
  "Like function `async-shell-command', but in the current Virtual
Dired directory.  Copied from `dired-smart-shell-command' from
dired-x"
  (interactive
   (list
    (read-shell-command "Async shell command: " nil nil
			(cond
			 (buffer-file-name (file-relative-name buffer-file-name))
			 ((eq major-mode 'dired-mode) (dired-get-filename t t))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (async-shell-command command output-buffer error-buffer)))
