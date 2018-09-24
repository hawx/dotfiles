;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c TAB") 'switch-to-previous-buffer)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-RET") 'newline)

(global-set-key (kbd "C-z") "")
(global-set-key (kbd "M-DEL") 'backward-kill-word)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(global-set-key (kbd "<f8>") 'next-error)

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

(global-set-key (kbd "<f1>") 'create-scratch-buffer)

(windmove-default-keybindings)
(global-unset-key (kbd "C-x o"))
