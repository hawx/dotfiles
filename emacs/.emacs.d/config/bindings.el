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

;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-apropos (:color blue :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))
(global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; M-g g
(defhydra hydra-goto-line (goto-map ""
                                    :pre (linum-mode 1)
                                    :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))
