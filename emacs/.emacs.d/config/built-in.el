(require 'cl)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in the tmp dir
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-name (concat temporary-file-directory "emacs-autosave-list"))

;; Remove scratch buffer message
(setq initial-scratch-message nil)

;; enable cua-mode for rectangular selections
(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

(setq visible-bell t
      column-number-mode t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      color-theme-is-global t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      confirm-nonexistent-file-or-buffer nil

      ;; Prefer left-right split
      split-height-threshold nil
      split-width-threshold 0)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq-default fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

;; remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; stop ffap from using strings beginning with /
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)
