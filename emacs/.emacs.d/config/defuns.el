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
  (let ((buffers (buffer-list)) (safe '("*scratch*")))
    (while buffers
      (when (not (member (car buffers) safe))
        (kill-buffer (car buffers))
        (setq buffers (cdr buffers))))))

(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(defun add-to-auto-mode-alist (mode values)
  "Adds the values to the the auto-mode-alist for mode."
  (let (v)
    (dolist (e values v)
      (add-to-list 'auto-mode-alist (cons e mode)))))

(defun load-lang-config (p)
  (load (concat dotfiles-config-dir "lang/" p)))

(defun load-lib-config (p)
  (load (concat dotfiles-config-dir "lib/" p)))

(defun add-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))
