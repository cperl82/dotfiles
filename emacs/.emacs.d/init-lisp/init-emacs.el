(use-package emacs
  :demand t
  :preface
  (defun cp/reduce-gc-cons-threshold ()
    (setq gc-cons-threshold (* 20 1000 1000)))
  :bind
  (("M-o" . other-window))
  :custom
  (ad-redefinition-action #'accept)
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux")))
  (column-number-mode t)
  (confirm-kill-emacs #'yes-or-no-p)
  (create-lockfiles nil)
  (copy-region-blink-predicate 'ignore)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (enable-recursive-minibuffers t)
  (gc-cons-threshold (* 100 1000 1000))
  (grep-find-use-xargs 'gnu-sort)
  (hs-isearch-open t)
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (load-prefer-newer t)
  ;; CR-someday cperl: consider nolittering
  (make-backup-files nil)
  (native-comp-async-report-warnings-errors nil)
  (scroll-preserve-screen-position t)
  (show-paren-delay 0)
  (split-window-preferred-function #'cp/split-window-function)
  (use-short-answers t)
  (window-combination-resize t)
  :config
  ;; Bindings for scrolling the window a line at a time. Useful when
  ;; you're on a given line of a function and want to scroll up to see
  ;; more of the function but don't want to move point.
  ;;
  ;; As far as I'm aware, I can't bind these with `bind' in
  ;; `use-package' as that uses `bind-key' and that doesn't support
  ;; macros as the target.
  (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
  (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
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
   (prog-mode . turn-on-auto-fill)
   (c-mode . cp/enable-hideshow-and-hide-all)))

(defun cp/enable-hideshow-and-hide-all ()
  (hs-minor-mode)
  (hs-hide-all))

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
