;; Visual Settings
(global-hl-line-mode 1) ; Vim cursorline
(setq display-fill-column-indicator-column 80) ; Ruler at 80
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode))) ; ^

;; Tab character enabling and disabling
(setq-default tab-width 2) ; 2 character as shift
(setq-default electric-indent-inhibit t) ; Disabling genius feature of indenting previous line

(defun disable-tabs () (setq indent-tabs-mode nil)) ; Space as tab
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(setq backward-delete-char-untabify-method 'hungry) ; Make backspace delete a whole character rather than once space

(add-hook 'prog-mode-hook 'disable-tabs) ; Space for most file types
(add-hook 'cmake-mode-hook 'enable-tabs) ; Tab for CMAKE

(setq-default java-indent-offset 4) ; Why Java
;; ------------------------------------

;; Displaying tab and trailing whitespace
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
;; --------------------------------------

;; Evil mode
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo) ; Vim style undo-redo sequence

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Custom evil keybindings figure this by using <C-h> k <key u want>
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ; C-g in insert is ESC
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char) ; Sorry you are not getting help in insert mode
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-visual-line)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "SPC h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "SPC j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "SPC k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "SPC l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "SPC w") 'delete-window)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk") ; Imagine requiring a separate package for this
(setq-default evil-escape-delay 0.2) ; Default 0,1 is too fast
;; ---------

