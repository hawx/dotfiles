;; http://www.emacswiki.org/emacs/IndentingJava
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

            (setq c-basic-offset 4)
            (subword-mode 1)))
