(use-package projectile
  :defer t
  :custom
  (projectile-enable-caching t)
  (projectile-cache-file
   (concat temporary-file-directory "projectile.cache"))
  (projectile-known-projects-file
   (concat temporary-file-directory "projectile-bookmarks.eld" ))
  (projectile-completion-system 'ivy)
  (projectile-project-name-function #'cp/projectile-project-name)
  (projectile-switch-project-action #'projectile-dired)
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "cscope.files")
  (projectile-mode))

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
