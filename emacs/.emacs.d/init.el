(require 'cask "/usr/local/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create variables to store the path to this dotfile dir's lib etc and tmp directories
(setq dotfiles-config-dir (concat dotfiles-dir "config/"))
(setq dotfiles-lib-dir   (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir   (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir   (concat dotfiles-dir "etc/"))

;; Create helper fns for loading dotfile paths and files
(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(add-to-list 'load-path dotfiles-lib-dir)

;; Pull in personalised config
(load-dotfile "config/core.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(purescript-mode-hook (quote (turn-on-purescript-indent)))
 '(standard-indent 2)
 '(typescript-indent-level 2 t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
