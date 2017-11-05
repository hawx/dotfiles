;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c TAB") 'switch-to-previous-buffer)

;; I can't deal with the default behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-RET") 'newline)

;; x and z are too close
(global-set-key (kbd "C-z") "")

;; others...
(global-set-key (kbd "M-DEL") 'backward-kill-word)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; BINDINGS TO REMEMBER
;;
;; "C-h b"  shows all key bindings
;; "M-m"    jumps to the first non-whitespace character on the line
;; "M-10 -" prints 10 dashes, obviously other numbers and chars work too
;; "M-^"    joins the current line to the previous
;; "M-:"    to eval something in mini-buffer
;; "M-;"    comments region
