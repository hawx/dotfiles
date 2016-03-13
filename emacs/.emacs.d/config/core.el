
;;; CORE

(load-dotfile "config/bindings.el")
(load-dotfile "config/built-in.el")
(load-dotfile "config/defuns.el")
(load-dotfile "config/theme.el")

;;; LIBS

(require 'smooth-scrolling)
(require 'rainbow-delimiters)
(require 'mwe-log-commands)
(require 'htmlize)

(load-lib-config "auto-complete")
(load-lib-config "auto-fill")
(load-lib-config "compile")
(load-lib-config "deft")
(load-lib-config "highlight")
(load-lib-config "ido")
(load-lib-config "paredit")
;; (load-lib-config "slime")
(load-lib-config "smex")
(load-lib-config "undo-tree")
(load-lib-config "textmate")
(load-lib-config "yasnippet")

(add-hook 'prog-mode-hook 'emr-initialize)
;; (require 'js2-refactor)

;;; LANGUAGES

(load-lang-config "ampl")
(load-lang-config "bnf")
(load-lang-config "clojure")
(load-lang-config "coffee")
(load-lang-config "css")
(load-lang-config "elixir")
(load-lang-config "go")
(load-lang-config "haml")
(load-lang-config "haskell")
(load-lang-config "jade")
(load-lang-config "java")
(load-lang-config "lisps")
(load-lang-config "markdown")
(load-lang-config "mustache")
(load-lang-config "nginx")
(load-lang-config "ruby")
(load-lang-config "sass")
(load-lang-config "shell")
(load-lang-config "slim")
(load-lang-config "tide")
(load-lang-config "vodka")
(load-lang-config "yaml")
(load-lang-config "web")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
