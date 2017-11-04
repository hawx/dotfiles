(autoload 'rust-mode "rust-mode" nil t)

(add-to-auto-mode-alist 'rust-mode '("\\.rs\\'"))

(setq rust-format-on-save t)
