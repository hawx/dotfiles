(defun elm-config/hook ()
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

(add-hook 'elm-mode-hook 'elm-config/hook)
