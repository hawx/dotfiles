(require 'color)
(show-paren-mode 1)
(auto-compression-mode t)
(setq-default auto-fill-mode 1)
(setq-default indent-tabs-mode nil)

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
  (setq deft-extension "md")
  (setq deft-directory "~/Documents/Notes/")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-auto-save-interval 0)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))

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
          ido-auto-merge-work-directories-length -1
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
  :defer 1
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
         ("C-c M-<" . mc/edit-lines)
         ("C-c M->" . mc/mark-all-like-this)
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
                          '(javascript-jshint json-jsonlint emacs-lisp-checkdoc typescript-tslint)))

    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

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

    (add-to-list 'flycheck-checkers 'my-typescript-tslint)))

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

(defun my/origami-focus ()
  "Show only the current node, but expand everything within that node."
  (interactive)
  (origami-show-only-node)
  (origami-open-node-recursively))

(use-package origami
  :init (global-origami-mode)
  :config (add-to-list 'origami-parser-alist '(typescript-mode . origami-c-style-parser))
  :bind (("C-c C-o" . origami-toggle-node)
         ("C-c C-e" . origami-close-node)
         ("C-c C-s" . origami-open-node)
         ("C-c C-a" . origami-open-all-nodes)
         ("C-c C-n" . origami-show-only-node)
         ("C-c C-m" . origami-open-node-recursively)))

(use-package hydra
  :config
  (progn
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
    (global-set-key (kbd "C-c t") 'hydra-toggle/body)))

(use-package shell-pop
  :init
  (setq shell-pop-universal-key "C-t")
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-full-span t))

(use-package winner
  :init
  (winner-mode))
