(require 'cl)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in the tmp dir
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-save-list-file-name
      (concat temporary-file-directory "emacs-autosave-list"))

;; When you visit a file, point goes to the last place where it was when you
;; previously visited
;; Save file is set to dotfiles-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)

;; enable cua-mode for rectangular selections
(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;; enable winner mode for C-c-(<left>|<right>) to navigate the history
;; of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))

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

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)

(setq-default fill-column 80)

;; Prefer left-right split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(setq diff-switches "-u")

(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(setq confirm-nonexistent-file-or-buffer nil)

;; remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)

;; Ignore files that fill up lists
(add-to-list 'completion-ignored-extensions "Icon\n") ;; this works, wierdly
(add-to-list 'completion-ignored-extensions ".DS_Store")
(add-to-list 'completion-ignored-extensions ".dropbox")

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar hawx/packages '(ac-slime
                        auto-complete
                        clojure-mode
                        coffee-mode
                        csharp-mode
                        deft
                        durendal
                        elixir-mode
                        git-commit-mode
                        go-mode
                        haml-mode
                        haskell-mode
                        highlight
                        htmlize
                        inf-ruby
                        jade-mode
                        js2-mode
                        markdown-mode
                        mode-compile
                        monokai-theme
                        mustache-mode
                        mwe-log-commands
                        nginx-mode
                        olivetti
                        paredit
                        popup
                        rainbow-delimiters
                        rainbow-mode
                        sass-mode
                        slim-mode
                        slime
                        slime-repl
                        slime-scratch
                        smex
                        smooth-scrolling
                        soothe-theme
                        textmate
                        twilight-theme
                        undo-tree
                        w3
                        yaml-mode
                        yasnippet)
  "My packages")
(package-initialize)

(defun hawx/packages-installed-p ()
  (loop for pkg in hawx/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (hawx/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg hawx/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
