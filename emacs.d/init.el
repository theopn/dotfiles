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

;; PACKAGE PACKAGE
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
;; ---------------

;; Load other config files
;; ----------------------------------------------------
(load "~/dotfiles/emacs.d/appearance.el")
(load "~/dotfiles/emacs.d/text_editor_fundamentals.el")
(load "~/dotfiles/emacs.d/convinience.el")
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
 '(whitespace-tab ((t (:foreground "#636363")))))
