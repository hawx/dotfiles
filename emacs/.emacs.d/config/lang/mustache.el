(add-vendor-path "mustache-mode")
(autoload 'mustache-mode "mustache-mode.el"
  "Major mode for editing Mustache templates" t)

(add-to-auto-mode-alist 'mustache-mode '("\\.mustache$"))
