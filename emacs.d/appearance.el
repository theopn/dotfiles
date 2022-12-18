;; Basic appearance settings
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(setq inhibit-startup-message t
      visible-bell t) ; Flash when bell rings
(menu-bar-mode -1) ; - disables menu bar
(tool-bar-mode -1) ; -1 disables tool bar
;; (set-fringe-mode 10) ; Vertical border -> Not compatible with non-GUI
(scroll-bar-mode -1) ; Disable scroll bar -> Not compatible with non-GUI
(set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font" :height 140) ; Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; Theme
(load-theme 'dracula t) ; ^
(set-frame-parameter (selected-frame) 'alpha '(90 90)) ; Transparency
(add-to-list 'default-frame-alist '(alpha 90 90)) ; ^
(defalias 'yes-or-no-p 'y-or-n-p) ; Change yes no menu
;; -------------------------

;; Apperance - Advanced
(use-package all-the-icons) ; Icons for Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(column-number-mode) ; Colume line in the Modeline
(global-display-line-numbers-mode t) ; Display line number
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) ;Don't display line number in the EShell
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; --------------------
