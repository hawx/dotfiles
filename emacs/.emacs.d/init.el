(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

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

(defun colorize-compilation-buffer ()
  (read-only-mode -1)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode +1))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
  (setq dired-use-ls-dired nil)

  (defun pbcopy ()
    (interactive)
    (call-process-region (point) (mark) "pbcopy")
    (setq deactivate-mark t))

  (defun pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-c c") 'pbcopy)
  (global-set-key (kbd "C-c v") 'pbpaste)
  (global-set-key (kbd "C-c x") 'pbcut))

(setq-default fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

;; remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'whitespace-cleanup)

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

(use-package delight
  :ensure t
  :init
  (delight 'eldoc-mode nil 'eldoc)
  (delight 'flymake-mode nil 'flymake)
  (delight 'whitespace-mode nil 'whitespace))

(use-package htmlize)

(use-package eval-sexp-fu
  :custom
  (eval-sexp-fu-flash-duration 0.3))

(use-package rainbow-delimiters
  :hook ((text-mode prog-mode) . rainbow-delimiters-mode))

(use-package subword
  :delight
  :init (global-subword-mode))

(use-package eglot
  :ensure t
  :defer t
  :delight
  :config
  (add-to-list 'eglot-server-programs '(hawx-gohtml-mode . ("html-languageserver" "--stdio")))
  :hook ((go-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (php-mode . eglot-ensure)
         (hawx-gohtml-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c ." . xref-find-references)
              ("C-c C-r" . eglot-rename)))

(use-package company
  :delight
  :init (global-company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay .3)
  (company-echo-delay 0)
  (company-begin-commands '(self-insert-command)))

(use-package undo-tree
  :delight
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,temporary-file-directory))))

(use-package deft
  :bind ("<f8>" . deft)
  :custom
  (deft-auto-save-interval 0)
  (deft-default-extension "md")
  (deft-directory "~/Documents/Notes")
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t))

(use-package ido
  :init
  (ido-mode t)
  :custom
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point 'guess)
  (ido-use-url-at-point nil)
  (ido-auto-merge-work-directories-length -1)
  (ido-max-prospects 10)
  :config
  (add-to-list 'ido-ignore-buffers "\\*ansi-term-1*")
  (add-to-list 'ido-ignore-buffers "\\*Deft*"))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :custom
  (csv-header-lines 1))

(use-package paredit
  :delight
  :hook ((emacs-lisp-mode lisp-mode scheme-mode ielm-mode clojure-mode) . paredit-mode)
  :bind (("C-f" . paredit-forward)
         ("C-b" . paredit-backward)))

(use-package smex
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (setq yas/root-directory (concat dotfiles-etc-dir "snippets"))
  (yas-load-directory yas/root-directory))

(use-package magit
  :config (setq magit-process-finish-apply-ansi-colors t)
  :delight magit-auto-revert-mode
  :delight auto-revert-mode
  :bind ("C-x g" . magit-status))

;; https://github.com/alphapapa/unpackaged.el#hydra
(use-package smerge-mode
  :after hydra
  :bind ("C-x ^" . unpackaged/smerge-hydra/body)
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package deadgrep
  :ensure-system-package (rg . ripgrep)
  :bind ("C-c s" . deadgrep))

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :custom-face
  (ivy-current-match ((t (:foreground "#8F9D6A"))))
  (ivy-subdir ((t (:foreground "#CF6A4C"))))
  (ivy-minibuffer-match-face-1 ((t (:background "#222222")))))

(use-package projectile
  :custom
  (projectile-mode-line-function '(lambda () (format " %s" (projectile-project-name))))
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
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
  :delight
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

(use-package flyspell-popup
  :bind ("C-c ;" . flyspell-popup-correct)
  :hook (flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(use-package hydra
  :config
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

;; (use-package add-node-modules-path
;;   :hook (js2-mode typescript-mode web-mode))

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
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook 'clojure-config/hook))

(use-package dockerfile-mode
  :mode "Dockerfile\\(?:\\..*\\)?\\'")

(use-package nginx-mode)

(use-package elm-mode
  :mode "\\.elm\\'"
  :after (company)
  :init
  (setq elm-format-on-save t)
  :config
  (add-to-list 'company-backends 'company-elm))

(defun hawx-eglot-organize-imports () (interactive)
	     (eglot-code-actions nil nil "source.organizeImports" t))

(use-package go-mode
  :mode "\\(\\.go\\|go\\.mod\\|go\\.sum\\)\\'"
  :ensure-system-package (gopls . "go install golang.org/x/tools/gopls@latest")
  :bind (:map go-mode-map
              ("C-x t f" . go-test-current-file)
              ("C-x t t" . go-test-current-test))
  :config
  (add-hook 'before-save-hook 'eglot-format-buffer)
  (add-hook 'before-save-hook 'hawx-eglot-organize-imports nil t))

(use-package haskell-mode
  :mode (("\\.hcr\\'" . ghc-core-mode)
         ("\\.dump-simpl\\'" . ghc-core-mode)
         ("\\.ghci\\'" . ghci-script-mode)
         ("\\.chs\\'" . haskell-c2hs-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.hsig\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . literate-haskell-mode)
         ("\\.hsc\\'" . haskell-mode)
         ("runghc" . haskell-mode)
         ("runhaskell" . haskell-mode))
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".o"))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.jsonld\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :ensure-system-package (javascript-typescript-langserver . "npm i -g javascript-typescript-langserver")
  :custom
  (js2-strict-trailing-comma-warning t)
  (js2-strict-inconsistent-return-warning nil)
  (js2-mode-show-strict-warnings nil)
  :config
  (unbind-key "M-." js2-mode-map)
  (unbind-key "C-c C-o" js2-mode-map)
  (unbind-key "C-c C-e" js2-mode-map)
  (unbind-key "C-c C-s" js2-mode-map)
  (unbind-key "C-c C-a" js2-mode-map)
  (unbind-key "C-c C-n" js2-mode-map)
  (unbind-key "C-c C-m" js2-mode-map)
  (unbind-key "C-c C-f" js2-mode-map))

(use-package rjsx-mode
  :mode "\\.jsx\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
         ("M-." . markdown-reference-goto-definition)
         ("M-," . markdown-reference-goto-link)
         ("C-c ." . markdown-reference-goto-link))
  :init (setq markdown-command "multimarkdown"))

(use-package ruby-mode
  :bind (:map ruby-mode-map ("RET" . reindent-then-newline-and-indent))
  :mode "\\.rake\\'"
  :mode "\\.gemspec\\'"
  :mode "\\.ru\\'"
  :mode "Rakefile"
  :mode "Gemfile"
  :mode "Capfile"
  :mode "Guardfile"
  :config
  (add-to-list 'completion-ignored-extensions ".rbc")
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package php-mode
  :ensure-system-package (intelephense . "npm i intelephense -g"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :ensure-system-package ((tsc . "npm i -g typescript")
                          (typescript-language-server . "npm i -g typescript-language-server"))
  :custom
  (typescript-indent-level 2))

(require 'vodka-mode)
(require 'hjson-mode)

(use-package web-mode
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.html?\\'"
  :mode "\\.gotmpl\\'"
  :mode "\\.handlebars\\'"
  :mode "\\.vue\\'"
  :mode "\\.twig\\'"
  :ensure-system-package ((css-languageserver . "npm i -g vscode-css-languageserver-bin")
                          (html-languageserver . "npm i -g vscode-html-languageserver-bin"))
  :custom
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t))

(define-derived-mode hawx-gohtml-mode web-mode "gohtml"
  "A major mode derived from web-mode for editing gohtml files")
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . hawx-gohtml-mode))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package terraform-mode)

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

(defalias 'list-buffers 'ibuffer)

;;; bindings
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*")))

;; https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun er-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; https://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun er-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


(windmove-default-keybindings)
(global-set-key (kbd "C-c TAB") 'switch-to-previous-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-RET") 'newline)
(global-set-key (kbd "C-z") "")
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "<f1>") 'create-scratch-buffer)
(global-set-key (kbd "<f5>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f6>") 'flymake-goto-next-error)
(global-set-key (kbd "C-c q") 'kill-this-buffer)
(global-set-key (kbd "C-c C-s") 'swiper-isearch)
(global-set-key (kbd "C-c M-d") 'er-delete-file-and-buffer)
(global-set-key (kbd "C-c M-r") 'er-rename-file-and-buffer)
(global-unset-key (kbd "C-x o"))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
