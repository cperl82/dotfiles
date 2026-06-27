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
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (enable-recursive-minibuffers t)
  (gc-cons-threshold (* 100 1000 1000))
  (grep-find-use-xargs 'gnu-sort)
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (load-prefer-newer t)
  ;; CR-someday cperl: consider nolittering
  (make-backup-files nil)
  (native-comp-async-report-warnings-errors nil)
  (show-paren-delay 0)
  (split-window-preferred-function #'cp/split-window-function)
  (use-short-answers t)
  (window-combination-resize t)
  :config
  (put 'narrow-to-region 'disabled nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-auto-revert-mode)
  (show-paren-mode)
  (winner-mode)
  :hook
  ((emacs-startup . cp/reduce-gc-cons-threshold)
   (emacs-startup . cp/startup-time)
   (prog-mode . turn-on-auto-fill)))

(defun cp/startup-time ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

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
