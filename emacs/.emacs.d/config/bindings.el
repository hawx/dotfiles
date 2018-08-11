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

(defun mode-is-on (name)
  (and (boundp name)
       (or (eq (eval name) 't)
           (eq (eval name) 1))))

(defhydra hydra-toggle (:color blue)
  "
_a_ auto-fill-mode         %(mode-is-on 'auto-fill-mode)
_e_ editorconfig-mode      %(mode-is-on 'editorconfig-mode)
_f_ follow-mode            %(mode-is-on 'follow-mode)
_o_ olivetti-mode          %(mode-is-on 'olivetti-mode)
_s_ subword-mode           %(mode-is-on 'subword-mode)
_t_ toggle-truncate-lines
_w_ whitespace-mode        %(mode-is-on 'whitespace-mode)
"
  ("a" auto-fill-mode)
  ("e" editorconfig-mode)
  ("f" follow-mode)
  ("o" olivetti-mode)
  ("s" subword-mode)
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode)
  ("q" nil "cancel"))
(global-set-key (kbd "C-c t") 'hydra-toggle/body)

(defhydra hydra-window-sizing ()
  ("[" shrink-window-horizontally)
  ("]" enlarge-window-horizontally)
  ("b" balance-windows :color blue)
  ("q" nil "cancel" :color blue))
(global-set-key (kbd "C-c w") 'hydra-window-sizing/body)
