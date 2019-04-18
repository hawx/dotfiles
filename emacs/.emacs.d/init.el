(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


(defvar dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(defvar dotfiles-lib-dir (concat dotfiles-dir "lib/"))
(defvar dotfiles-etc-dir (concat dotfiles-dir "etc/"))

(add-to-list 'load-path dotfiles-lib-dir)


(setq gc-cons-threshold 20000000)

;;; utf8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;; built-in

;; better display for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; colorise ansi codes
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq
 ;; Put autosave files (ie #foo#) and backup files (ie foo~) in the tmp dir
 ;; store all backup and autosave files in the tmp dir
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 auto-save-list-file-name (concat temporary-file-directory "emacs-autosave-list")

 ;; Remove scratch buffer message
 initial-scratch-message nil
 initial-major-mode 'text-mode

 visible-bell t
 ring-bell-function 'ignore
 column-number-mode t
 echo-keystrokes 0.1
 font-lock-maximum-decoration t
 inhibit-startup-message t
 shift-select-mode nil
 require-final-newline t
 truncate-partial-width-windows nil
 delete-by-moving-to-trash nil
 confirm-nonexistent-file-or-buffer nil

 ;; Prefer left-right split
 split-height-threshold nil
 split-width-threshold 0)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq-default fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

;; remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; stop ffap from using strings beginning with /
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

(setq ffap-machine-p-known 'reject)

;;; theme
(use-package twilight-theme
  :config
  (load-theme 'twilight t))

(ansi-color-for-comint-mode-on)

; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ? ))

;;; lib
(require 'color)
(show-paren-mode 1)
(auto-compression-mode t)
(cua-selection-mode t)

(setq-default auto-fill-mode 1)
(setq-default indent-tabs-mode nil)

(use-package diminish)
(diminish 'eldoc-mode)

(use-package ag
  :config
  (setq ag-reuse-window 't))

(use-package htmlize)

(use-package eval-sexp-fu
  :config
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package rainbow-delimiters
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package subword
  :diminish
  :init (global-subword-mode))

(use-package company
  :diminish
  :init (global-company-mode)
  :config
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-begin-commands '(self-insert-command)))

(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :bind (("C-c ." . lsp-find-references)
         ("C-c C-r" . lsp-rename))
  :custom
  (lsp-prefer-flymake nil)
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil))

(use-package company-lsp
  :commands company-lsp)

(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode))

(use-package deft
  :bind ("<f8>" . deft)
  :custom
  (deft-auto-save-interval 0)
  (deft-default-extension "md")
  (deft-directory "~/Documents/Notes")
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t))

;; Things I forget:
;; - Use 'C-j' to use typed text verbatim
;; - Use 'C-d' to open current directory in dired
(use-package ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-url-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-max-prospects 10)
  (ido-mode t)
  :config
  (add-to-list 'ido-ignore-buffers "\\*tide-server*")
  (add-to-list 'ido-ignore-buffers "\\*ansi-term-1*")
  (add-to-list 'ido-ignore-buffers "\\*Deft*"))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package csv-mode
  :custom
  (csv-header-lines 1))

(use-package paredit
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package smex
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (setq yas/root-directory (concat dotfiles-etc-dir "snippets"))
  (yas-load-directory yas/root-directory))

(use-package magit
  :config (setq magit-process-finish-apply-ansi-colors t)
  :bind ("C-x g" . magit-status))

(use-package deadgrep
  :bind ("C-c s" . deadgrep))

(use-package projectile
  :init
  (setq projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  (setq projectile-indexing-method 'alien)
  :defer 1
  :bind (("C-c p" . hydra-projectile/body)
         ("C-c f" . projectile-find-file)
         ("C-c b" . projectile-switch-to-buffer))
  :config
  (define-key projectile-mode-map (kbd "C-c P") 'projectile-command-map)

  (add-to-list 'projectile-globally-ignored-files "node-modules")

  ;; https://github.com/abo-abo/hydra/wiki/Projectile
  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra hydra-projectile (:color teal :hint nil)
    "
PROJECTILE: %(projectile-project-root)

"
    ("d"   projectile-find-dir "dir")
    ("F"   projectile-find-file-in-directory "file curr dir")
    ("i"   projectile-ibuffer "ibuffer")
    ("K"   projectile-kill-buffers "kill all buffers")
    ("s"   projectile-switch-project "switch project")
    ("b"   projectile-build-project "build project")
    ("t"   projectile-test-project "test project")
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue))

  (projectile-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode 1))

(use-package multiple-cursors
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c M-<" . mc/edit-lines)
         ("C-c M->" . mc/mark-all-like-this)
         ("C-c m" . multiple-cursors-hydra/body))
  :config
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)))

(use-package olivetti
  :config
  (setq olivetti-hide-mode-line t))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  ;; disable some checkers
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-python-json emacs-lisp-checkdoc typescript-tslint)))

  (setq flycheck-checker-error-threshold 1000)

  (flycheck-def-config-file-var flycheck-my-typescript-tsconfig
      my-typescript-tslint "tsconfig.json"
    :safe #'stringp
    :package-version '(flycheck . "27"))

  (flycheck-def-config-file-var flycheck-my-typescript-tslint-config
      my-typescript-tslint "tslint.json"
    :safe #'stringp
    :package-version '(flycheck . "27"))

  (flycheck-define-checker my-typescript-tslint
    "TypeScript style checker using TSLint."
    :command ("tslint" "--format" "json"
              (config-file "--config" flycheck-my-typescript-tslint-config)
              (config-file "--project" flycheck-my-typescript-tsconfig)
              source-inplace)
    :error-parser flycheck-parse-tslint
    :modes (typescript-mode))

  (add-to-list 'flycheck-checkers 'my-typescript-tslint))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/origami-focus (buffer point)
  "Show only the current node, but expand everything within that node."
  (interactive (list (current-buffer) (point)))
  (origami-show-only-node buffer point)
  (origami-open-node-recursively buffer point))

(use-package flyspell-popup
  :bind ("C-c ;" . flyspell-popup-correct)
  :hook (flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(use-package origami
  :init (global-origami-mode)
  :config (add-to-list 'origami-parser-alist '(typescript-mode . origami-c-style-parser))
  :bind (("C-c C-o" . origami-toggle-node)
         ("C-c C-e" . origami-close-node)
         ("C-c C-s" . origami-open-node)
         ("C-c C-a" . origami-reset)
         ("C-c C-b" . my/origami-focus)
         ("C-c C-n" . origami-show-only-node)
         ("C-c C-m" . origami-open-node-recursively)))

(use-package hydra
  :config
  ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
  (defhydra hydra-apropos (:color blue :hint nil)
    "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
    ("a" apropos)
    ("d" apropos-documentation)
    ("v" apropos-variable)
    ("c" apropos-command)
    ("l" apropos-library)
    ("u" apropos-user-option)
    ("e" apropos-value))
  (global-set-key (kbd "C-c h") 'hydra-apropos/body)

  (defun mode-is-on (name)
    (and (boundp name)
         (or (eq (eval name) 't)
             (eq (eval name) 1))))

  (defhydra hydra-toggle (:color blue)
    "
_a_ auto-fill-mode         %(mode-is-on 'auto-fill-mode)
_e_ editorconfig-mode      %(mode-is-on 'editorconfig-mode)
_f_ follow-mode            %(mode-is-on 'follow-mode)
_o_ olivetti-mode          %(mode-is-on 'olivetti-mode)
_s_ subword-mode           %(mode-is-on 'subword-mode)
_t_ toggle-truncate-lines
_w_ whitespace-mode        %(mode-is-on 'whitespace-mode)
"
    ("a" auto-fill-mode)
    ("e" editorconfig-mode)
    ("f" follow-mode)
    ("o" olivetti-mode)
    ("s" subword-mode)
    ("t" toggle-truncate-lines)
    ("w" whitespace-mode)
    ("q" nil "cancel"))
  (global-set-key (kbd "C-c t") 'hydra-toggle/body))

(defun my/projectile-shell-pop ()
  "Open shell-pop for projectile project"
  (interactive)
  (let (old-default-directory (shell-pop-default-directory))
    (setq shell-pop-default-directory (projectile-project-root))
    (call-interactively 'shell-pop)
    (setq shell-pop-default-directory (old-default-directory))))

(use-package shell-pop
  :init
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-full-span t)
  (setq shell-pop-universal-key "C-t"))

(use-package winner
  :init
  (winner-mode))

(use-package expand-region
  :bind (("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

;;; lang
(setq-default standard-indent 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)

(use-package add-node-modules-path
  :config
  (add-node-modules-path)
  :hook (js2-mode typescript-mode))

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

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'clojure-config/hook))

(use-package coffee-mode
  :defer t)

(use-package dockerfile-mode)

(use-package nginx-mode)

(use-package elm-mode
  :after (company)
  :init
  (setq elm-format-on-save t)
  :config
  (add-to-list 'company-backends 'company-elm))

(defun go-config/hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs")))

(use-package golint)

(use-package go-mode
  :mode "\\(\\.go\\|go.mod\\|go.sum\\)\\'"
  :bind (:map go-mode-map
              ("C-x t f" . go-test-current-file)
              ("C-x t t" . go-test-current-test))
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'go-mode-hook 'go-config/hook))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".o"))

(use-package json-mode)

(use-package js2-mode
  :after (add-node-modules-path)
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-strict-trailing-comma-warning t)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (unbind-key "C-c C-o" js2-mode-map)
  (unbind-key "C-c C-e" js2-mode-map)
  (unbind-key "C-c C-s" js2-mode-map)
  (unbind-key "C-c C-a" js2-mode-map)
  (unbind-key "C-c C-n" js2-mode-map)
  (unbind-key "C-c C-m" js2-mode-map)
  (unbind-key "C-c C-f" js2-mode-map))

(use-package eslint-fix
  :after (js2-mode)
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'eslint-fix nil t))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :bind (:map ruby-mode-map ("RET" . reindent-then-newline-and-indent))
  :mode "\\.rake$"
  :mode "\\.gemspec$"
  :mode "\\.ru$"
  :mode "Rakefile"
  :mode "Gemfile"
  :mode "Capfile"
  :mode "Guardfile"
  :config
  (add-to-list 'completion-ignored-extensions ".rbc")
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package sass-mode)

(use-package scss-mode)

(use-package sh-script
  :mode "\\.sh$"
  :mode "\\.zsh$"
  :mode "zshrc"
  :config
  (setq sh-basic-offset 2))

(require 'tslint-fix)

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(require 'vodka-mode)
(require 'hjson-mode)

(defun web-config/hook ()
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t))

(use-package web-mode
  :defer t
  :mode "\\.erb$"
  :mode "\\.mustache$"
  :mode "\\.html?$"
  :mode "\\.gotmpl$"
  :mode "\\.handlebars$"
  :config (add-hook 'web-mode-hook 'web-config/hook))

(use-package yaml-mode)

(defalias 'list-buffers 'ibuffer)

;;; bindings
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

(windmove-default-keybindings)
(global-set-key (kbd "C-c TAB") 'switch-to-previous-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-RET") 'newline)
(global-set-key (kbd "C-z") "")
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "<f1>") 'create-scratch-buffer)
(global-set-key (kbd "C-c q") 'kill-this-buffer)
(global-unset-key (kbd "C-x o"))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
