(add-vendor-path "slim-mode")
(autoload 'slim-mode "slim-mode.el"
  "Major mode for editing Slim files" t)

(add-to-auto-mode-alist 'slim-mode '("\\.slim$"))
