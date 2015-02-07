(add-vendor-path "yaml-mode")
(autoload 'yaml-mode "yaml-mode.el"
  "Major mode for editing YAML files" t)

(add-to-auto-mode-alist 'yaml-mode '("\\.yml$" "\\.yaml$"))
