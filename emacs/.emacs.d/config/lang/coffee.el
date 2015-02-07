(autoload 'coffee-mode "coffee-mode.el"
  "Major mode for editing CoffeeScript files" t)

(add-to-auto-mode-alist 'coffee-mode '("\\.coffee$" "Cakefile"))
