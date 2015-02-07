(load-theme 'twilight t)

(global-hl-line-mode 1)
(ansi-color-for-comint-mode-on)

; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
