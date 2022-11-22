;;------------------------------
;;   __     By Theo
;;  /__\ __ ___   __ _  ___ ___
;; /_\| '_ ` _ \ / _` |/ __/ __|
;;//__| | | | | | (_| | (__\__ \
;;\__/|_| |_| |_|\__,_|\___|___/
;;------------------------------

;; a.k.a my to-do list

;; Change backup directory
(setq backup-directory-alist `(("." . "~/emacs_backup"))
      backup-by-copying-when-linked t ; For symlinks I think?
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Basic appearance settings
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
;;

;; Essential packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; For non-Linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
;;

;; Helper packages
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))
;;

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
;;

;; Load other config files
;; ----------------------------------------------------
(load "~/dotfiles/emacs.d/text_editor_fundamentals.el")
(load "~/dotfiles/emacs.d/org_config.el")
;; ----------------------------------------------------

;; Automatically puts buffer list and Org agenda
(defun startup-layout ()
 (interactive)
 (delete-other-windows)
 (org-agenda-list)
 (split-window-horizontally)
 (next-multiframe-window)
 (list-buffers)
 (enlarge-window-horizontally 10)
 )
(startup-layout)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key use-package rainbow-delimiters ivy-rich hydra general evil doom-modeline counsel all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
