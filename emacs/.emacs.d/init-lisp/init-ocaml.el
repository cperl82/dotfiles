(use-package tuareg
  :defer t
  :config
  (add-to-list
     'hs-special-modes-alist
     `(tuareg-mode
       ,cp/tuareg-mode-hs-start-regexp nil nil  cp/tuareg-mode-hs-forward-sexp-fun))
  :hook
  ((tuareg-mode . cp/enable-hideshow-and-hide-all)))

(setq cp/tuareg-mode-hs-start-regexp
      (rx (or
           (: bow "module" eow
              (1+ whitespace)
              (or
               (: (1+ (not whitespace))
                  (1+ whitespace)
                  (or
                   (: "=" (1+ whitespace) bow "struct" eow)
                   (: ":" (1+ whitespace) bow "sig" eow)))
               (: bow "type" eow
                  (1+ whitespace)
                  (1+ (not whitespace))
                  (1+ whitespace)
                  "="
                  (1+ whitespace)
                  bow "sig" eow)))
           (: bow "end" eow
              (1+ whitespace)
              "="
              (1+ whitespace)
              bow "struct" eow)
           (: bow "let" eow
              (1+ whitespace))
           (: bow "and" eow
              (1+ whitespace))
           (: bow "let%" (1+ (not whitespace)) eow
              (1+ whitespace))
           (: bow "type" eow
              (+? (1+ whitespace) (1+ (not whitespace)))
              (1+ whitespace)
              "="))))

(defun cp/tuareg-mode-hs-forward-sexp-fun (arg)
  (forward-line 1)
  (re-search-forward
   (rx-to-string
    `(or (: bol (** 0 ,(current-column) whitespace) (not whitespace))
         (: eos))
    t))
  (beginning-of-line)
  (back-to-indentation)
  (if (not
       (or
        (looking-at "in")
        (looking-at "end")
        (looking-at ";;")
        (looking-at "with")
        (eq (point) (buffer-size))))
      (progn
        (re-search-backward (rx bol (0+ whitespace) (not whitespace) any))
        (move-end-of-line nil))))
