;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Things that Doom does by default that Emacs don't
;; - jk to ESC
;; - Tab character highlighting
;; - Spell checker
;; - Changing backup directory - Doom does in ~/.emacs.d/.local/
[ ; Same as (when nil _)
(setq backup-directory-alist `(("." . "~/.emacs_backup"))
      backup-by-copying-when-linked t ; For symlinks I think?
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
]
;;


(setq user-full-name "Theo Park"
      user-mail-address "no.email.for.you@theo.com")

;; `doom-big-font' may be used for presentation
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font" :size 14) ; `describe-font', `eval-region', `doom/reload-font'
      doom-variable-pitch-font (font-spec :family "FantasqueSansMono Nerd Font" :size 15)) ; non-monospace font
(setq doom-theme 'doom-dracula) ; `doom-theme' or `load-theme'

(setq display-line-numbers-type 'relative) ; t/nil for regular on and off
(column-number-mode) ; Colume line in the Modeline

(set-frame-parameter (selected-frame) 'alpha '(90 90)) ; Transparency
(add-to-list 'default-frame-alist '(alpha 90 90)) ; ^
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode +1) ; Enable whitespace mode everywhere

;(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
(after! org
        (setq org-ellipsis " ▾") ; S-TAB view icon
        (setq org-log-done 'time) ; Record the time stamp of when things were done
        (setq org-log-into-drawer t) ; Idk what it does, but I heard it's related to repeating task organizatioon
        (setq org-agenda-start-with-log-mode nil) ; This will display bunch of time stamp on the agenda by default, not a big fan
        (setq org-agenda-window-setup 'current-window) ; Lauch Org agenda on a current window, needed to launch one on the startup
        (setq org-agenda-skip-timestamp-if-done t) ; Don't show DONE item on the agenda
        (setq org-agenda-skip-scheduled-if-done t) ; ^
        (setq org-agenda-skip-deadline-if-done t) ; ^
        (add-hook 'auto-save-hook 'org-save-all-org-buffers) ; Auto save every 30 seconds

        (require 'org-habit)
        (add-to-list 'org-modules 'org-habit)
        (setq org-habit-graph-column 60)

        (setq org-agenda-files
                '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org/educatio.org"
                "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org/habitus.org"
                "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org/officium.org"
                "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org/proiecta_gaudia.org"
                "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org/vita.org"))

        ;; Archive can be acheived with C-c C-w
        (setq org-refile-targets ; Default current buffer + archive.org file
        '(("archive.org" :maxlevel . 2))) ; Detects upto heading 2, so make month (heading 1) and categories (heading 2)
        (advice-add 'org-refile :after 'org-save-all-org-buffers) ; Automatically save after refiling

        (setq org-todo-keywords
        '((sequence "TODO(t)" "INPR(i)" "NEXT(n)" "|" "DONE(d)" "CANC(c)")))

        (setq org-agenda-span 8 ; Agenda shows 8 day
        org-deadline-warning-days 2
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d") ; Shows 3 days before

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
        )))) ; Honestly never uses it until I figure out how to launch it by default
) ; after! ends


;; ---------------

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; DOOM DASHBOARD ASCII BANNER
(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '(
  ""
  ""
  "" ; https://www.asciiart.eu/animals/cats - I added a few layers of belly and it's not my cat Oliver
  "                         .---....___                             "
  "                __..--''``          `` _..._    __               "
  "      /// //_.-'    .-/';  `         `<._  ``.''_ `. / // /      "
  "     ///_.-' _..--.'_    ;                    `( ) ) // //       "
  "     / (_..-' // (< _     ;_..__               ; `' / ///        "
  "      / // // //  `-._,_)' // / ``--...____..-' /// / //         "
  ""
  ""
  "" ; patorjk.com featured Figlet font - Soft
  "  ,--.  ,--.                                                     "
  ",-'  '-.|  ,---.  ,---.  ,---. ,--,--,--. ,--,--. ,---. ,---.    "
  "'-.  .-'|  .-.  || .-. :| .-. ||        |' ,-.  || .--'(  .-'    "
  "  |  |  |  | |  |\\   --.' '-' '|  |  |  |\\ '-'  |\\ `--..-'  `)"
  "  `--'  `--' `--' `----' `---' `--`--`--' `--`--' `---'`----'    "
  ""
  ""
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
