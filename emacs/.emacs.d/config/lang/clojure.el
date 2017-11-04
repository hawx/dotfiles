(defun clojure-config/hook ()
  (rainbow-delimiters-mode)

  (font-lock-add-keywords nil `(("(\\(fn\\)[\[[:space:]]"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "λ")
                                           nil)))))

  (font-lock-add-keywords nil `(("\\(#\\)("
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "ƒ")
                               nil)))))
  (font-lock-add-keywords nil `(("\\(#\\){"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "∈")))))))

(add-hook 'clojure-mode-hook 'clojure-config/hook)
