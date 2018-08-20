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

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(defvar dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(defvar dotfiles-config-dir (concat dotfiles-dir "config/"))
(defvar dotfiles-lib-dir    (concat dotfiles-dir "lib/"))
(defvar dotfiles-tmp-dir    (concat dotfiles-dir "tmp/"))
(defvar dotfiles-etc-dir    (concat dotfiles-dir "etc/"))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(add-to-list 'load-path dotfiles-lib-dir)

(load-dotfile "config/utf8.el")
(load-dotfile "config/built-in.el")
(load-dotfile "config/defuns.el")
(load-dotfile "config/theme.el")
(load-dotfile "config/lib.el")
(load-dotfile "config/lang.el")
(load-dotfile "config/bindings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (shell-pop add-node-modules-path tide eslint-fix origami json-mode highlight-indent-guides ag yaml-mode web-mode scss-mode sass-mode rust-mode inf-ruby markdown-mode js2-mode haskell-mode golint go-guru go-eldoc company-go elm-mode coffee-mode clojure-mode hydra olivetti multiple-cursors editorconfig projectile magit yasnippet smex paredit deft undo-tree company rainbow-delimiters eval-sexp-fu htmlize twilight-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#0ccc0ccc0ccc"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
