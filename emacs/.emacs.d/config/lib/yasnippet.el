(require 'yasnippet)

(yas-global-mode 1)

(setq yas/root-directory (concat dotfiles-etc-dir "snippets"))
(yas-load-directory yas/root-directory)
