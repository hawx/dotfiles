(require 'go-mode)

(autoload 'go-mode "go-mode"
  "Major mode for the Go programming language" t)

(add-to-auto-mode-alist 'go-mode '("\\.go$"))

(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; gotest bindings
  (define-key go-mode-map (kbd "C-x t f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-x t t") 'go-test-current-test)

  ;; autocomplete
  (require 'go-autocomplete)
  (require 'auto-complete-config)

  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
  (require 'golint))

(add-hook 'go-mode-hook 'go-mode-setup)
(add-hook 'go-mode-hook 'subword-mode)
