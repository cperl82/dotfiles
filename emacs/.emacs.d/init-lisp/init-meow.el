(use-package meow
  :straight t
  :demand t
  :preface
  ;; Add bindings to normal mode for c-mode functions that insert into
  ;; the buffer.
  ;;
  ;; Meow intercepts anything that is bound to `self-insert-command',
  ;; which handles most things, but in c-mode several keys are bound
  ;; to these functions, so we need to remap them to `undefined' in
  ;; normal mode, else they'll insert into the buffer (or do whatever
  ;; magic thing they're supposed to do).
  (defun cp/meow-normal-remap-symbols-undefined (symbols)
    (dolist (sym symbols)
      (define-key meow-normal-state-keymap (vector 'remap sym) #'undefined)))
  (defun cp/meow-normal-remap-c-mode ()
      (cp/meow-normal-remap-symbols-undefined
       '(c-electric-pound
         c-electric-brace
         c-electric-slash
         c-electric-star
         c-electric-semi&comma
         c-electric-colon
         c-electric-paren)))
  :hook
  ((c-mode . cp/meow-normal-remap-c-mode))
  :custom
  (meow-expand-hint-remove-delay 0)
  (meow-selection-command-fallback
   '((meow-kill . meow-delete)
     (meow-change . meow-change-char)))
  :config
  ;; Unbind one of the several bindings to `text-scale-adjust' as this one
  ;; get's in the way of using meow's fallback behavior for SPC x 0 to
  ;; invoke `delete-window'
  (global-unset-key (kbd "C-x C-0"))

  ;; We're doing this here rather than in the hideshow use-package
  ;; block because this function has meow specific functionality,
  ;; e.g. checking if we're in normal mode
  (defun cp/meow-hide-show-toggle ()
    (interactive)
    (if (and (bound-and-true-p meow-normal-mode)
             (not (use-region-p)))
        (hs-toggle-hiding)
      (let* ((hs-minor-mode nil)
	     (f (key-binding (this-command-keys))))
	(call-interactively f))))
  (with-eval-after-load 'hideshow
    (define-key hs-minor-mode-map (kbd "TAB") #'cp/meow-hide-show-toggle))

  (defun cp/meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("w" . flash-jump)
     '("f g" . cp/counsel-rg)
     '("f f" . cp/counsel-rg-files))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("o" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . comment-or-uncomment-region)
    ;'("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("O" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
    ;'("o" . meow-block)
    ;'("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("d" . meow-kill)
     '("t" . meow-till)
     '("f" . meow-find)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
     '("RET" . ignore)
     )
    )
  (cp/meow-setup)

  (meow-global-mode 1))

(use-package vundo
  :straight t
  :defer t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package flash
  :straight (:host github :repo "Prgebish/flash" :ref "42fbc58")
  :defer t)

(use-package embrace
  :straight t
  :defer t)
