(require 'cl)
(require 'slime)

(slime-setup '(slime-repl slime-scratch))
(setq slime-protocol-version 'ignore)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

;; ac-slime auto-complete plugin
(add-lib-path "ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
