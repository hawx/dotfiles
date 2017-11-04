(autoload 'purescript-mode "purescript-mode.el"
  "Major mode for editing PureScript files" t)

(add-to-auto-mode-alist 'purescript-mode '("\\.purs$"))

(remove-hook 'purescript-mode-hook)
