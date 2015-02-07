(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun turn-on-paredit ()
  (paredit-mode t))

(defun turn-on-rainbow ()
  (rainbow-delimiters-mode))

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-rainbow))
