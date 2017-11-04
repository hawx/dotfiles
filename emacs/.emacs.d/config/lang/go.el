(defun go-config/hook ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; gotest bindings
  (define-key go-mode-map (kbd "C-x t f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-x t t") 'go-test-current-test)

  ;; autocomplete
  (require 'go-autocomplete)
  (require 'auto-complete-config)

  (add-to-list 'load-path
               (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
  (require 'golint)

  (require 'go-guru)
  (go-guru-hl-identifier-mode))

(add-hook 'go-mode-hook 'go-config/hook)
(add-to-auto-mode-alist 'go-mode '("\\.go$"))
