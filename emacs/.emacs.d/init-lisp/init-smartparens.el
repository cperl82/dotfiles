(use-package smartparens
  :straight t
  :demand t
  :preface
  (defun cp/smartparens-mode-setup ()
    (smartparens-strict-mode)
    (sp-use-smartparens-bindings))
  :config
  ;; Taken from https://ebzzry.com/en/emacs-pairs/
  (defmacro cp/sp-defpairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (cp/sp-defpairs ((paren . \"(\")
                   (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun
                       ,(read
                         (concat
                          "cp/wrap-with-"
                          (prin1-to-string key)
                          "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))
  (cp/sp-defpairs ((paren . "(")
                   (bracket . "[")
                   (brace . "{")
                   (single-quote . "'")
                   (double-quote . "\"")
                   (back-quote . "`")))
  (sp-pair "("  nil :wrap "C-c s (")
  (sp-pair "["  nil :wrap "C-c s [")
  (sp-pair "{"  nil :wrap "C-c s {")
  (sp-pair "'"  nil :wrap "C-c s '")
  (sp-pair "\"" nil :wrap "C-c s \"")
  (sp-pair "`"  nil :wrap "C-c s `")
  (sp-with-modes '(emacs-lisp-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
  ;; `sp-local-pair' checks to see if x-mode-map exists and then
  ;; bindes the :wrap binding in that map if it does. Since we defer
  ;; loading of org via use-package, we have to wait until it's loaded
  ;; to execute these local pairs
  (with-eval-after-load 'org
    (sp-with-modes '(org-mode)
      (sp-local-pair "*" "*" :wrap "C-c s *")
      (sp-local-pair "~" "~" :wrap "C-c s ~")
      (sp-local-pair "_" "_" :wrap "C-c s _")))
  :hook
  ((lisp-data-mode . cp/smartparens-mode-setup)
   (org-mode . cp/smartparens-mode-setup)))
