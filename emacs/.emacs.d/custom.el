(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-dont-blink-commands '(next-line previous-line forward-line recenter-top-bottom))
 '(beacon-mode t)
 '(custom-safe-themes
   '("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default))
 '(deft-auto-save-interval 0 t)
 '(deft-default-extension "md" t)
 '(deft-directory "~/Documents/Notes" t)
 '(deft-use-filename-as-title t t)
 '(deft-use-filter-string-for-filename t t)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "  " mode-line-position mode-line-modes
     (vc-mode vc-mode)
     mode-line-misc-info " " mode-line-end-spaces))
 '(package-selected-packages
   '(lsp-ivy sqlformat groovy-mode ws-butler counsel-projectile counsel ivy terraform-mode feature-mode php-mode prettier-js beacon use-package-ensure-system-package csharp-mode rjsx-mode flycheck go-mode typescript-mode expand-region dockerfile-mode company-lsp lsp-ui lsp-mode diminish nginx-mode twilight-theme edit-indirect deadgrep company-quickhelp flyspell-popup flyspell-correct flx-ido csv-mode shell-pop add-node-modules-path origami json-mode highlight-indent-guides ag yaml-mode web-mode scss-mode sass-mode rust-mode inf-ruby markdown-mode js2-mode haskell-mode golint elm-mode clojure-mode hydra olivetti multiple-cursors editorconfig projectile magit yasnippet smex paredit deft undo-tree company rainbow-delimiters eval-sexp-fu htmlize twilight-theme use-package))
 '(safe-local-variable-values
   '((backup-by-copying t)
     (lsp-enable-file-watchers nil)
     (my/inhibit-eslint-fix quote t)
     (lsp-before-save-edits))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#f8f8f8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Iosevka"))))
 '(company-tooltip ((t (:inherit default :background "#0ccc0ccc0ccc"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip-scrollbar-track ((t (:background "#199919991999"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(ivy-current-match ((t (:foreground "#8F9D6A"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#222222"))))
 '(ivy-subdir ((t (:foreground "#CF6A4C")))))
