(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun sh ()
  "Open a shell with (mostly) working colour support"
  (interactive)
  (ansi-term "/usr/local/bin/zsh"))

(defun clean-slate ()
  "Kills all buffers except *scratch*"
  (interactive)
  (let ((buffers (buffer-list)) (safe '("*scratch*" "*Messages*")))
    (while buffers
      (when (not (member (car buffers) safe))
        (kill-buffer (car buffers))
        (setq buffers (cdr buffers))))))
