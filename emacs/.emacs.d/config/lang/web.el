(require 'web-mode)

(defun my-web-mode-hook ()
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")))

(add-to-auto-mode-alist 'web-mode
                        '("\\.erb\\'"
                          "\\.mustache\\'"
                          "\\.html?\\'"))
