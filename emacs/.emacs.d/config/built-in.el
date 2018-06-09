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

;; When you visit a file, point goes to the last place where it was when you
;; previously visited.
;; Save file is set to dotfiles-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)

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
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (concat dotfiles-tmp-dir "places"))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(auto-compression-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)

(setq-default fill-column 80)

;; Prefer left-right split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(setq diff-switches "-u")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(setq confirm-nonexistent-file-or-buffer nil)

;; remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; stop ffap from using strings beginning with /
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)
