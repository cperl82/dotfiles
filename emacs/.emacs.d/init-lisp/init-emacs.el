(use-package emacs
  :demand t
  :preface
  (defun cp/reduce-gc-cons-threshold ()
    (setq gc-cons-threshold (* 20 1000 1000)))
  :custom
  (ad-redefinition-action #'accept)
  (column-number-mode t)
  (confirm-kill-emacs #'yes-or-no-p)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (gc-cons-threshold (* 100 1000 1000))
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (load-prefer-newer t)
  (make-backup-files nil)
  (split-window-preferred-function #'cp/split-window-function)
  (use-short-answers t)
  (window-combination-resize t)
  :config
  (when (< emacs-major-version 27)
    (package-initialize))
  (when (> emacs-major-version 28)
    (setq native-comp-async-report-warnings-errors nil))
  (put 'narrow-to-region 'disabled nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-auto-revert-mode)
  :hook
  ((prog-mode . turn-on-auto-fill)
   (emacs-startup . cp/reduce-gc-cons-threshold)))

(defun cp/split-window-function (&rest r)
  "When there is one window, split it horizontally unless the frame is
smaller than 120.

  If there are multiple windows, don't split anything."
  (let ((width (frame-text-width))
        (nwindows (length (window-list))))
    (if (= nwindows 1)
        (if (< width 120) (split-window-below) (split-window-right))
      nil)))

(defun cp/revert-buffer-all ()
  "Revert all buffers.  This reverts buffers that are visiting a file, kills
  buffers whose visited file has disappeared and refreshes dired
  buffers."
  (interactive)
  (if (y-or-n-p "Revert ALL buffers? ")
      (save-excursion
        (dolist (b (buffer-list))
          (set-buffer b)
          (cond
           (buffer-file-name
            (if (file-exists-p buffer-file-name)
                (revert-buffer t t t)
              (kill-buffer b)))
           ((eq major-mode 'dired-mode) (revert-buffer t t t)))))))

(provide 'init-emacs)
