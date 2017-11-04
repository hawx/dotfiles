(defun clojure-config/hook ()
  (rainbow-delimiters-mode)

  (font-lock-add-keywords nil `(("(\\(fn\\)[\[[:space:]]"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "λ")
                                           nil)))))

  (font-lock-add-keywords nil `(("\\(#\\)("
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "ƒ")
                               nil)))))
  (font-lock-add-keywords nil `(("\\(#\\){"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "∈")))))))

(use-package clojure-mode
  :defer t
  :ensure t
  :config (add-hook 'clojure-mode-hook 'clojure-config/hook))

(use-package coffee-mode
  :defer t
  :ensure t)

(defun elm-config/hook ()
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

(use-package elm-mode
  :defer t
  :ensure t
  :config (add-hook 'elm-mode-hook 'elm-config/hook))

(defun go-config/hook ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; gotest bindings
  (define-key go-mode-map (kbd "C-x t f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-x t t") 'go-test-current-test)

  (require 'company-go)

  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
  (require 'golint)

  (require 'go-guru)
  (go-guru-hl-identifier-mode))

(use-package go-mode
  :defer t
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-config/hook)
  (add-hook 'go-mode-hook 'subword-mode))

(use-package haskell-mode
  :defer t
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".o"))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; START RUBY
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
;; END RUBY

(use-package rust-mode
  :defer t
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package sass-mode
  :defer t
  :ensure t)

(use-package scss-mode
  :defer t
  :ensure t)

(use-package sh-script
  :defer t
  :init
  (add-to-auto-mode-alist 'shell-script-mode '("\\.sh$" "\\.zsh$" "zshrc$"))
  :config
  (setq sh-indentation 2
        sh-basic-offset 2))

(defun typescript-config/hook ()
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (tide-setup)
  (flycheck-mode 5)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (company-mode-on))

(use-package tide
  :defer t
  :ensure t
  :config (add-hook 'typescript-mode-hook 'typescript-config/hook))

(require 'vodka-mode)

(defun web-config/hook ()
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t))

(use-package web-mode
  :defer t
  :ensure t
  :init
  (add-to-auto-mode-alist 'web-mode
                          '("\\.erb\\'"
                            "\\.mustache\\'"
                            "\\.html?\\'"))
  :config
  (add-hook 'web-mode-hook 'web-config/hook))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
