(add-vendor-path "clojure-mode")
(autoload 'clojure-mode "clojure-mode.el"
  "Major mode for editing Clojure files" t)

(add-to-auto-mode-alist 'clojure-mode '("\\.clj$"))

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(add-lib-path "durendal")

(eval-after-load 'clojure-mode
  '(progn
     (require 'durendal)
     (durendal-enable)))
