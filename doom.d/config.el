;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Cat: https://www.asciiart.eu/animals/cats
;;      Jonathan added a few layers of belly so it looks more like my cat Oliver
;; Logo: figlet -f small Theomacs
;;       soft.flf file can be found in my "Theovim" repository
;;  "            \/       \/              "
;;  "            /\_______/\              "
;;  "           /   o   o   \             "
;;  "          (  ==  ^  ==  )            "
;;  "           )           (             "
;;  "          (             )            "
;;  "          ( (  )   (  ) )            "
;;  "         (__(__)___(__)__)           "
;;  ""
;;  "  _   _                              "
;;  " | |_| |_  ___ ___ _ __  __ _ __ ___ "
;;  " |  _| ' \/ -_) _ \ '  \/ _` / _(_-< "
;;  "  \__|_||_\___\___/_|_|_\__,_\__/__/ "
;;  ""

;; Things that Doom does by default that Emacs don't
;; - jk for ESC
;; - Tab character highlighting
;; - Spell checker
;; - Changing backup directory - Doom does in ~/.emacs.d/.local/

(when nil ; Evaluates to nothing
 (setq backup-directory-alist `(("." . "~/.emacs_backup")) ; Backup is handled by Doom
       backup-by-copying-when-linked t ; For symlinks I think?
       delete-old-versions t
       kept-new-versions 6
       kept-old-versions 2
       version-control t)
  )

;; Info
(setq user-full-name "Theo Park"
      user-mail-address "no.email.for.you@theo.com")

;; Apperance
(setq doom-theme 'doom-dracula) ; `doom-theme' or `load-theme'
;; `doom-big-font' may be used for presentation
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 13) ; `describe-font', `eval-region', `doom/reload-font'
      doom-variable-pitch-font (font-spec :family "CaskaydiaCove Nerd Font" :size 13)) ; non-monospace font

(setq frame-title-format "%b - Theomacs :)") ; Window title

(setq display-line-numbers-type 'relative) ; t/nil for regular on and off
(setq scroll-margin 10) ; Vim scrolloff

(column-number-mode) ; Colume line in the Modeline
(display-time) ; Display the time on the modeline
(setq display-time-day-and-date t ; Date on the modeline
      display-time-24hr-format t) ; 24 hour

(set-frame-parameter (selected-frame) 'alpha '(90 90)) ; Transparency
(add-to-list 'default-frame-alist '(alpha 90 90)) ; ^

;; File editing
(setq tab-width 2) ; 2 character as tab
(setq display-fill-column-indicator-column 80) ; ruler 80
(setq backward-delete-char-untabify-method 'hungry) ; Make backspace delete a whole character rather than once space

(setq whitespace-style '(face tabs tab-mark trailing)) ; Trailing whitespace
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363"))))) ; White space appears as red
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode +1) ; Enable whitespace mode everywhere

;; Go to the matching paren if on a paren; otherwise insert %
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Custom evil keybindings figure this by using <C-h> k <key u want>
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ; C-g in insert is ESC
(define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char) ; Sorry you are not getting help in insert mode
(define-key evil-insert-state-map (kbd "C-j") 'evil-next-visual-line)
(define-key evil-insert-state-map (kbd "C-k") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
;; C-x o (other-window)
;; C-x { } enlarge/shrink-window-horizontally C-x ^ enlarge-window
;; C-x 0 (delete-window)
;; C-x 2 (split-window-below) C-x 3 (split-window-right)
(define-key evil-normal-state-map (kbd "SPC w") 'delete-window)

;; Org config
(load "~/dotfiles/doom.d/org-config.el")

;; Doom Dashboard ascii banner
(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '(
            ""
            "            \\/       \\/            "
            "            /\\_______/\\            "
            "           /   o   o   \\            "
            "          (  ==  ^  ==  )            "
            "           )           (             "
            "          (             )            "
            "          ( (  )   (  ) )            "
            "         (__(__)___(__)__)           "
            ""
            "  _   _                              "
            " | |_| |_  ___ ___ _ __  __ _ __ ___ "
            " |  _| ' \\/ -_) _ \\ '  \\/ _` / _(_-< "
            "  \\__|_||_\\___\\___/_|_|_\\__,_\\__/__/ "
            ""
            ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

;; EOF
