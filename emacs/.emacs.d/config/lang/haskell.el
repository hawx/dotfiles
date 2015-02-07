(add-vendor-path "haskell-mode")
(autoload 'haskell-mode "haskell-mode.el"
  "Major mode for editing Haskell files" t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-auto-mode-alist 'haskell-mode '("\\.hs$"))

(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
