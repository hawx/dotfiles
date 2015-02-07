(add-vendor-path "nginx-mode")
(autoload 'nginx-mode "nginx-mode.el"
  "Major mode for editing nginx config files" t)

(add-to-list 'auto-mode-alist '("/home/hawx/dev/config/files/etc/nginx/sites-available/.*" . nginx-mode))
