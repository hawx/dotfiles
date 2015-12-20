(add-vendor-path "markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-auto-mode-alist 'markdown-mode
                        '("\\.md$" "\\.markdown$" "\\.text$"))
