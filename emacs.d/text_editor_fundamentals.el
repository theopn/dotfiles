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

(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere

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

  ;; More or less necessary bindings
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ; C-g in insert is ESC
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char) ; C-h in insert is left arrow

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;;

