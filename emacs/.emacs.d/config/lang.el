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
  :config (add-hook 'clojure-mode-hook 'clojure-config/hook))

(use-package coffee-mode
  :defer t)

(defun elm-config/hook ()
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

(use-package elm-mode
  :defer t
  :config (add-hook 'elm-mode-hook 'elm-config/hook))

(defun go-config/hook ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  (set (make-local-variable 'company-backends) '(company-go))
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))

  (go-guru-hl-identifier-mode))

(use-package company-go)
(use-package go-eldoc)
(use-package go-guru)
(use-package golint)

(use-package go-mode
  :defer t
  :bind (:map go-mode-map
              ("C-x t f" . go-test-current-file)
              ("C-x t t" . go-test-current-test))
  :config
  (add-hook 'go-mode-hook 'go-config/hook))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".o"))

(use-package json-mode)

(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-strict-trailing-comma-warning t)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (unbind-key "C-c C-o" js2-mode-map)
  (unbind-key "C-c C-e" js2-mode-map)
  (unbind-key "C-c C-s" js2-mode-map)
  (unbind-key "C-c C-a" js2-mode-map)
  (unbind-key "C-c C-n" js2-mode-map)
  (unbind-key "C-c C-m" js2-mode-map))

(use-package markdown-mode)

(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :bind (:map ruby-mode-map ("RET" . reindent-then-newline-and-indent))
  :mode "\\.rake$"
  :mode "\\.gemspec$"
  :mode "\\.ru$"
  :mode "Rakefile"
  :mode "Gemfile"
  :mode "Capfile"
  :mode "Guardfile"
  :config
  (add-to-list 'completion-ignored-extensions ".rbc")
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package sass-mode)

(use-package scss-mode)

(use-package sh-script
  :mode "\\.sh$"
  :mode "\\.zsh$"
  :mode "zshrc"
  :config
  (setq sh-indentation 2
        sh-basic-offset 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :after (js2-mode typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
         (js2-mode . setup-tide-mode)))

(use-package typescript-mode)

(require 'vodka-mode)
(require 'hjson-mode)

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
  :mode "\\.erb$"
  :mode "\\.mustache$"
  :mode "\\.html?$"
  :config (add-hook 'web-mode-hook 'web-config/hook))

(use-package yaml-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
