(require 'color)

(setq-default auto-fill-mode 1)

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
  :bind ("<f8>" . deft)
  :config
  (setq deft-extension "txt")
  (setq deft-directory "~/Documents/Notes/")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t))

;; Things I forget:
;; - Use 'C-j' to use typed text verbatim
;; - Use 'C-d' to open current directory in dired
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
  :bind (("C-c p" . hydra-projectile/body)
         ("C-c f" . hydra-projectile/body))
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c P")))
  :config
  (progn
    (setq projectile-mode-line '(:eval (format "​[%s]" (projectile-project-name))))

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

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
  _f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
  _d_: dir             _g_: update gtags      _b_: switch to buffer  _x_: remove known project
  _F_: file curr dir   _o_: multi-occur       _K_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current

"
      ("f"   projectile-find-file)
      ("d"   projectile-find-dir)
      ("F"   projectile-find-file-in-directory)
      ("r"   projectile-recentf)

      ("a"   projectile-ag)
      ("g"   ggtags-update-tags)
      ("o"   projectile-multi-occur)

      ("i"   projectile-ibuffer)
      ("b"   projectile-switch-to-buffer)
      ("K"   projectile-kill-buffers)

      ("c"   projectile-invalidate-cache)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)

      ("s"   projectile-switch-project "switch project")
      ("`"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))

    (projectile-global-mode)))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package multiple-cursors
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c m" . multiple-cursors-hydra/body))
  :config
  (progn
    (defhydra multiple-cursors-hydra (:hint nil)
      "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))))

(use-package olivetti
  :config
  (setq olivetti-hide-mode-line t))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    ;; disable some checkers
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint json-jsonlint)))

    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

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

(use-package hydra)
