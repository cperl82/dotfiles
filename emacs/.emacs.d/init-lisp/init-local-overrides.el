(condition-case nil
    (load-file (expand-file-name "~/.emacs.local"))
  (file-error nil))
(provide 'init-local-overrides)
