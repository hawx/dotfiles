;; (require 'cask "/usr/local/cask/cask.el")
;; (cask-initialize)
;; (require 'pallet)
;; (pallet-mode t)

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq dotfiles-config-dir (concat dotfiles-dir "config/"))
(setq dotfiles-lib-dir    (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir    (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir    (concat dotfiles-dir "etc/"))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(add-to-list 'load-path dotfiles-lib-dir)
(load-dotfile "config/index.el")
