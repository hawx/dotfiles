;;; CORE

(load-dotfile "config/bindings.el")
(load-dotfile "config/built-in.el")
(load-dotfile "config/defuns.el")
(load-dotfile "config/theme.el")
(load-dotfile "config/lib.el")

;;; LANGUAGES

(load-lang-config "ampl")
(load-lang-config "bnf")
(load-lang-config "clojure")
(load-lang-config "coffee")
(load-lang-config "elm")
(load-lang-config "go")
(load-lang-config "haskell")
(load-lang-config "java")
(load-lang-config "lisps")
(load-lang-config "ruby")
(load-lang-config "sass")
(load-lang-config "shell")
(load-lang-config "tide")
(load-lang-config "vodka")
(load-lang-config "web")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
