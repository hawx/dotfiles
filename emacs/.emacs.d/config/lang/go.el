(require 'go-mode)

(autoload 'go-mode "go-mode"
  "Major mode for the Go programming language" t)

(add-to-list 'completion-ignored-extensions ".6")
(add-to-auto-mode-alist 'go-mode '("\\.go$"))

(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; autocomplete
  (require 'go-autocomplete)
  (require 'auto-complete-config))

(add-hook 'go-mode-hook 'go-mode-setup)
