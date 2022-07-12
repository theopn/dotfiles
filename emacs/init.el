;;------------------------------
;;   __     By Theo
;;  /__\ __ ___   __ _  ___ ___
;; /_\| '_ ` _ \ / _` |/ __/ __|
;;//__| | | | | | (_| | (__\__ \
;;\__/|_| |_| |_|\__,_|\___|___/
;;------------------------------

;; a.k.a my to-do list

;; ---[[ Essential ackages
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
;; --]]

;; ---[[ Helper packages
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
;; --]]

;; ---[[ Apperance - Basic
(setq inhibit-startup-message t
      visible-bell t) ; Flash when bell rings
(menu-bar-mode 1) ; Enable menu bar
(set-fringe-mode 10) ; Vertical border
(scroll-bar-mode -1) ; Disable scroll bar
(set-face-attribute 'default nil :font "Comic Mono" :height 140)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; Theme
(load-theme 'dracula t) ;
(set-frame-parameter (selected-frame) 'alpha '(90 90)) ; Transparency
(add-to-list 'default-frame-alist '(alpha 90 90)) ;
(defalias 'yes-or-no-p 'y-or-n-p) ; Change yes no menu
;; --]]

;; ---[[ Apperance - Advanced
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(column-number-mode)
(global-display-line-numbers-mode t) ; Display line number
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; --]]

;; ---[[ Evil mode
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;; --]]

;; ---[[ Org Mode Apperance
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))
;; --]]

;; ---[[ Org Mode Environment
;; Org mode file directory
(defvar my_org_dir '"~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-window-setup 'current-window) 
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/officium.org"
          "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/educatio.org"
	  "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/proiecta_gaudia.org")))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "INPR(i)" "NEXT(n)" "|" "DONE(d)" "CANC(c)")))

(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 3)))
      (todo "TODO"
            ((org-agenda-overriding-header "TO-DO")
             (org-agenda-files org-agenda-files)))
      (todo "INPR"
            ((org-agenda-overriding-header "In Progress")
	     (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "NEXT"
            ((org-agenda-overriding-header "Next")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
))))
;; --]]

;; Emacs is Jack of all trades, master of one.
(defun startup-layout ()
 (interactive)
 (delete-other-windows)
 (split-window-horizontally)
 (next-multiframe-window)
 (org-agenda-list)
 (next-multiframe-window)
 (dired my_org_dir)
 )
(startup-layout)
