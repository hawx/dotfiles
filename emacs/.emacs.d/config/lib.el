(require 'color)

(setq-default auto-fill-mode 1)

(use-package htmlize)

(use-package eval-sexp-fu
  :config
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package rainbow-delimiters
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-begin-commands '(self-insert-command))

  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 5)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package deft
  :config
  (setq deft-extension "txt")
  (setq deft-directory "~/Documents/Notes/")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (global-set-key [f8] 'deft))

(use-package ido
  :init
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-url-at-point nil
          ido-max-prospects 10)
    (ido-mode t)))

(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode)))

(use-package smex
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas/root-directory (concat dotfiles-etc-dir "snippets"))
  (yas-load-directory yas/root-directory))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  (projectile-global-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package multiple-cursors
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)))
