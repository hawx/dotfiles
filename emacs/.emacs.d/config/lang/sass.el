(autoload 'sass-mode "sass-mode.el"
  "Major mode for editing Sass files" t)

(add-to-auto-mode-alist 'sass-mode '("\\.sass$" "\\.scss$"))
