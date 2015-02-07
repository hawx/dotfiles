(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by 'mode compile'" t)

(global-set-key (kbd "C-c c") 'mode-compile)
(global-set-key (kbd "C-c k") 'mode-compile-kill)

(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion (recompile)))

(global-set-key (kbd "C-c c") 'recompile-quietly)
