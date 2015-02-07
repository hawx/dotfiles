(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
(byte-compile-init-dir)

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create variables to store the path to this dotfile dir's lib etc and tmp directories
(setq dotfiles-config-dir (concat dotfiles-dir "config/"))
(setq dotfiles-lib-dir   (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir   (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir   (concat dotfiles-dir "etc/"))
(setq dotfiles-theme-dir (concat dotfiles-dir "themes/"))
(setq dotfiles-vendor-dir (concat dotfiles-dir "vendor/"))

;; Create helper fns for loading dotfile paths and files
(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(defun add-load-path (p)
  (add-to-list 'load-path p))

(add-load-path dotfiles-lib-dir)
(add-load-path dotfiles-theme-dir)


;; Pull in personalised config
(load-dotfile "config/core.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(standard-indent 2))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
