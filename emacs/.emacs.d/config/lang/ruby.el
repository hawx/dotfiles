(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

(add-to-auto-mode-alist
 'ruby-mode '("\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile" "Gemfile" "Capfile"
              "Guardfile"))

; We never want to edit Rubinus bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

; From http://stackoverflow.com/questions/4412739/emacs-ruby-mode-indentation-behavior
; to fix indenting `method :bar => true,\n:foo => false` type things
(defadvice ruby-indent-line (after line-up-args activate)
  (let (indent prev-indent arg-indent)
    (save-excursion
      (back-to-indentation)
      (when (zerop (car (syntax-ppss)))
        (setq indent (current-column))
        (skip-chars-backward " \t\n")
        (when (eq ?, (char-before))
          (ruby-backward-sexp)
          (back-to-indentation)
          (setq prev-indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          (setq arg-indent (current-column)))))
    (when prev-indent
      (let ((offset (- (current-column) indent)))
        (cond ((< indent prev-indent)
               (indent-line-to prev-indent))
              ((= indent prev-indent)
               (indent-line-to arg-indent)))
                (when (> offset 0) (forward-char offset))))))


; M-x run-ruby
(require 'inf-ruby)

(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
