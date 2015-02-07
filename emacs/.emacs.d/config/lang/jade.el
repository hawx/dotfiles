(add-vendor-path "jade-mode")

(autoload 'jade-mode "jade-mode"
  "Major mode for editing jade templates" t)

(add-to-auto-mode-alist 'jade-mode '("\\.jade$"))

(autoload 'sws-mode "sws-mode"
  "Major mode for editing stylus templates" t)

(add-to-auto-mode-alist 'sws-mode '("\\.styl$"))
