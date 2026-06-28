(use-package escreen
  :defer t
  :straight nil
  :load-path "lisp"
  :commands (escreen-create-screen)
  :general
  (:keymaps '(escreen-map)
   "C-b" nil
   "TAB" #'escreen-goto-last-screen
   "e"   #'cp/escreen-show-active-screens
   "v"   #'cp/escreen-show-active-screens-vertical
   "f"   #'cp/escreen-show-active-screens-horizontal
   "V"   #'cp/escreen-set-show-active-screens-fun-vertical
   "F"   #'cp/escreen-set-show-active-screens-fun-horizontal
   "A"   #'cp/escreen-set-show-active-screens-fun-auto
   "r"   #'cp/escreen-rename-screen
   "s"   #'cp/escreen-switch-to-screen-with-ivy-completion
   "C"   #'cp/escreen-compress
   "k"   #'escreen-kill-screen
   "H"   #'cp/escreen-move-screen-left
   "L"   #'cp/escreen-move-screen-right
   "p"   #'escreen-goto-prev-screen
   "n"   #'escreen-goto-next-screen
   "h"   #'escreen-goto-prev-screen
   "l"   #'escreen-goto-next-screen)
  :config
  (setq escreen-max-screens 30)
  (advice-add 'escreen-goto-screen   :after #'cp/advice/escreen-goto-screen)
  (advice-add 'escreen-kill-screen   :after #'cp/advice/escreen-kill-screen)
  (advice-add 'escreen-create-screen :after #'cp/advice/escreen-create-screen)
  (advice-add 'escreen-install       :after #'cp/advice/escreen-install)
  (escreen-install))

(defun cp/escreen-swap-screen (a &optional b)
  (when (and (numberp a) (numberp b))
    (let* ((current-screen-number (escreen-get-current-screen-number))
           (b (or b current-screen-number))
           (screen-data-a (escreen-configuration-escreen a))
           (screen-data-b (escreen-configuration-escreen b)))
      (when
          (and
           (not (equal a b))
           (>= a 0) (<= a escreen-highest-screen-number-used)
           (>= b 0) (<= b escreen-highest-screen-number-used))
        (cond ((and screen-data-a screen-data-b)
               ;; Both screens exist
               (setcar screen-data-a b)
               (setcar screen-data-b a))
              ((and screen-data-a (not screen-data-b))
               ;; The other screen doesn't exist
               (setcar screen-data-a b)))
        (cond ((equal current-screen-number a) (setq escreen-current-screen-number b))
              ((equal current-screen-number b) (setq escreen-current-screen-number a))
              (t t))))))

(defun cp/escreen-move-screen (direction screen-number n)
  (let* ((screen-to-move (or screen-number (escreen-get-current-screen-number)))
         (amount-to-move (or n 1))
         (other-screen-number
          (cond ((eq direction 'left)  (- screen-to-move amount-to-move))
                ((eq direction 'right) (+ screen-to-move amount-to-move)))))
    (cond ((and
            (>= other-screen-number 0)
            (<= other-screen-number escreen-highest-screen-number-used))
           (cp/escreen-swap-screen screen-to-move other-screen-number))
          ;; These are the cases where we're moving right off the
          ;; right end or left off the left end
          ((< other-screen-number 0)
           (let ((other-screen-number
                  (+ escreen-highest-screen-number-used other-screen-number)))
             (cp/escreen-swap-screen screen-to-move other-screen-number)))
          ((> other-screen-number escreen-highest-screen-number-used)
           (let ((other-screen-number
                  (- other-screen-number (1+ escreen-highest-screen-number-used))))
             (cp/escreen-swap-screen screen-to-move other-screen-number)))))
  (cp/escreen-show-active-screens))

(defun cp/escreen-move-screen-left (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'left screen-number n))

(defun cp/escreen-move-screen-right (&optional screen-number n)
  (interactive)
  (cp/escreen-move-screen 'right screen-number n))

(defun cp/escreen-rename-screen (&optional name number suppress-message)
  (interactive "sNew screen name: ")
  (let ((screen-data
         (escreen-configuration-escreen (or number escreen-current-screen-number)))
        (new-name (cond ((equal name "") nil)
                        ((stringp name) name)
                        (t "default"))))
    (setcar (cdr screen-data) new-name)
    (when (not suppress-message)
      (cp/escreen-show-active-screens))))

(defun cp/escreen-propertize-screen-number (number)
  (let ((star (propertize "*" 'face 'font-lock-string-face)))
    (cond ((eq escreen-current-screen-number number) (format "%d%s" number star))
          (t (format "%d-" number)))))

(defvar cp/escreen-show-active-screens-fun #'cp/escreen-show-active-screens-auto)

(defun cp/escreen-set-show-active-screens-fun-gen (how)
  (let ((val
         (cond
          ((eq how 'horizontal) #'cp/escreen-show-active-screens-horizontal)
          ((eq how 'vertical) #'cp/escreen-show-active-screens-vertical)
          (t #'cp/escreen-show-active-screens-auto))))
    (setq cp/escreen-show-active-screens-fun val)))

(defun cp/escreen-set-show-active-screens-fun-horizontal ()
  (interactive)
  (message "Setting escreen to always display horizontal")
  (cp/escreen-set-show-active-screens-fun-gen 'horizontal))

(defun cp/escreen-set-show-active-screens-fun-vertical ()
  (interactive)
  (message "Setting escreen to always display vertical")
  (cp/escreen-set-show-active-screens-fun-gen 'vertical))

(defun cp/escreen-set-show-active-screens-fun-auto ()
  (interactive)
  (message "Setting escreen to pick horizontal/vertical automatically")
  (cp/escreen-set-show-active-screens-fun-gen t))

(defvar cp/escreen-show-active-screens-clear-timer nil)

(defun cp/escreen-show-active-screens-gen (how &optional vertical-clear-delay)
  (when cp/escreen-show-active-screens-clear-timer
    (cancel-timer cp/escreen-show-active-screens-clear-timer))
  (let* ((delayed-clear
          (lambda ()
            (run-with-timer (or vertical-clear-delay 2) nil (lambda () (message nil)))))
         (format-screens
          (lambda (format-str join-str)
            (->>
             (cp/escreen-configuration-screen-numbers-and-names)
             (-sort (lambda (s1 s2) (< (car s1) (car s2))))
             (-map
              (lambda (screen)
                (let ((number (car screen))
                      (name   (cdr screen)))
                  (format format-str (cp/escreen-propertize-screen-number number) name))))
             (s-join join-str))))
         (h-args '("%s %s" "  "))
         (v-args
          (list (if (<= (length escreen-configuration-alist) 10) "%2s %s" "%3s %s") "\n"))
         (string
          (cond
           ((eq how 'horizontal) (apply format-screens h-args))
           ((eq how 'vertical)
            (progn
              (setq cp/escreen-show-active-screens-clear-timer (apply delayed-clear ()))
              (apply format-screens v-args)))
           (t
            (let ((horizontal (apply format-screens h-args)))
              (if (< (length horizontal) (frame-width))
                  horizontal
                (progn
                  (setq cp/escreen-show-active-screens-clear-timer (apply delayed-clear ()))
                  (apply format-screens v-args))))))))
    (message "%s" string))
  nil)

(defun cp/escreen-show-active-screens-horizontal ()
  (interactive)
  (cp/escreen-show-active-screens-gen 'horizontal))

(defun cp/escreen-show-active-screens-vertical ()
  (interactive)
  (cp/escreen-show-active-screens-gen 'vertical))

(defun cp/escreen-show-active-screens-auto ()
  (interactive)
  (cp/escreen-show-active-screens-gen t))

(defun cp/escreen-show-active-screens ()
  (interactive)
  (funcall cp/escreen-show-active-screens-fun))

(defun cp/escreen-configuration-screen-numbers-and-names ()
  (-map
   (lambda (entry) `(,(nth 0 entry) . ,(nth 1 entry)))
   escreen-configuration-alist))

(defun cp/escreen-ivy-screen-number-to-datum (width n)
  ;; CR-soon cperl: This could use some cleaning and rethinking of the interface
  (let* ((screen-data (escreen-configuration-escreen n))
         (name (escreen-configuration-screen-name screen-data))
         (n-windows (length (escreen-configuration-data-map screen-data)))
         (s
          (if width
              (let* ((number (cp/escreen-propertize-screen-number n))
                     (fmt (format
                           (concat
                            (if (<= (length escreen-configuration-alist) 10) "%2s" "%3s")
                            " "
                            "%%-%ds"
                            " "
                            "(%%d buffers)")
                           number
                           width))
                     (n-buffers
                      (->> screen-data
                           (nth 3)
                           (--map (nth 0 it))
                           (--map (nth 1 it))
                           (length))))
                (format fmt name n-buffers))
            (format "%d:%s" n name))))
    `(,s . ,n)))

(defun cp/escreen-ivy-collection ()
  (let* ((current (escreen-get-current-screen-number))
         (numbers-and-names (cp/escreen-configuration-screen-numbers-and-names))
         (all-but-current (-filter (lambda (x) (not (equal (car x) current))) numbers-and-names))
         (width (-reduce-from (lambda (a b) (max a (length (cdr b)))) 0 all-but-current)))
    (->>
     (-map 'car all-but-current)
     (-sort '<)
     (-map (lambda (n) (cp/escreen-ivy-screen-number-to-datum width n))))))

(defun cp/escreen-ivy-action (selected)
  (escreen-goto-screen (cdr selected))
  (cp/escreen-show-active-screens))

(defun cp/escreen-switch-to-screen-with-ivy-completion ()
  (interactive)
  (let ((collection (cp/escreen-ivy-collection)))
    (if (> (length collection) 0)
        (let* ((current (car (cp/escreen-ivy-screen-number-to-datum nil (escreen-get-current-screen-number))))
               (prompt (format "switch to escreen [%s]:" current)))
          (ivy-read prompt collection
                    :require-match t
                    :action #'cp/escreen-ivy-action))
      (cp/escreen-show-active-screens))))

(defun cp/escreen-compress ()
  "Compress all screen numbers to remove gaps"
  (interactive)
  (-each-indexed
      (-sort '< (escreen-configuration-screen-numbers))
    (lambda (idx screen-number)
      (let ((shift (- screen-number idx)))
        (progn
          (message "cp/escreen-compress shifting screen %d left by %d" screen-number shift)
          (cp/escreen-move-screen-left screen-number shift)))))
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-goto-screen (n &optional dont-update-current)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-kill-screen (&optional n)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-create-screen (&optional n)
  (cp/escreen-rename-screen)
  (cp/escreen-show-active-screens))

(defun cp/advice/escreen-install ()
  (cp/escreen-rename-screen nil nil t))
