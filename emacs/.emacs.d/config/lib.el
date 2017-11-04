(require 'color)

(setq-default auto-fill-mode 1)

(use-package htmlize
  :ensure t)

(use-package eval-sexp-fu
  :ensure t
  :config
  (setq eval-sexp-fu-flash-duration 0.5))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package subword
  :ensure t
  :diminish subword-mode)

(use-package company
  :ensure t
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
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package deft
  :ensure t
  :defer t
  :config
  (setq deft-extension "txt")
  (setq deft-directory "~/Documents/Notes/")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (global-set-key [f8] 'deft))

(use-package ido
  :ensure t
  :defer t
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
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode)))

(use-package smex
  :ensure t
  :defer t
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (yas-global-mode 1)
  :config
  (setq yas/root-directory (concat dotfiles-etc-dir "snippets"))
  (yas-load-directory yas/root-directory))
