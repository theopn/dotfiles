;;; $DOOMDIR/org-config.el -*- lexical-binding: t; -*-

;; Org Mode configuration for my Doom Emacs environment
;; Author: Theo Park
;; Created: 2023-05-xx
;; Keywords: Emacs, Doom Emacs, Org Mode
;; Copyright (C) 2022-2023 Theo Park
;; This file is licensed under MIT license. See <https://github.com/theopn/dotfiles>

;; Variables
(setq org-directory "~/My Drive/l1-cache/theo-org")
(setq org-roam-directory "~/My Drive/l1-cache/theo-org-roam")

;; Make sure Doom initialize the base config (keybindings, etc.) first
(after! org
  ;; Org syntax
  (setq org-ellipsis " ▾") ; S-TAB view icon
  (setq org-log-done 'time) ; Record the time stamp of when things were done
  (setq org-log-into-drawer t) ; Make "logbook" property for recurring tasks
  (setq org-agenda-start-with-log-mode nil) ; This will display bunch of time stamp on the agenda by default, not a big fan
  (setq org-agenda-window-setup 'current-window) ; Lauch Org agenda on a current window, needed to launch one on the startup
  (setq org-agenda-skip-timestamp-if-done t) ; Don't show DONE item on the agenda
  (setq org-agenda-skip-scheduled-if-done t) ; ^
  (setq org-agenda-skip-deadline-if-done t) ; ^

  (add-hook 'auto-save-hook 'org-save-all-org-buffers) ; Auto save every 30 seconds

  ;; Org keyword
  ;; TODO: tasks that require my effort
  ;; IDEA: unpolished task ideas captured using Org Capture
  ;; RMDR: Reminder, tasks that I simply have to check if it was complete (usually recurring)
  ;; DONE: task was completed successfully
  ;; CANC: task was not completed successfully
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IDEA(i)" "RMDR(r)" "|" "DONE(d)" "CANC(c)")))

  ;; Org agenda
  (setq org-agenda-files
        '("capture.org"
          "education.org"
          "habits.org"
          "projects.org"
          "vita.org"
          "work.org"))

  (setq org-agenda-span 8 ; Agenda shows 8 day
        org-deadline-warning-days 2
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d") ; Shows 3 days before

  ;; Configure custom agenda view with unspecified todo items from Org Capture (capture.org)
  (setq org-agenda-custom-commands
        '(("d" "Theo's Dashboard"
           ((todo "IDEA"
                  ((org-agenda-overriding-header "Captured Ideas/Tasks")
                   (org-agenda-files org-agenda-files)))
            (agenda "")
            ))))


  ;; Org capture
  (setq org-default-notes-file (concat org-directory "/capture.org")) ; Default capture file
  (setq org-capture-templates
        '(("t" "Todo/Idea Capture"
           entry (file+headline org-default-notes-file "Task Ideas")
           ;; %u/%U: Inactive timestamp, %t/%T: Active timestamp, %a: Annotation, %?: Cursor
           "* IDEA %?\n %U\n %a" :empty-lines-after 1)
          ("w" "Writing Idea"
           entry (file+headline org-default-notes-file "Writing Ideas")
           "* %?\n" :empty-lines 1)))

  ;; Archive with C-c C-w
  (setq org-refile-targets ; Default current buffer + archive.org file
        '(("archive.org" :maxlevel . 2))) ; Detects upto heading 2, so make month (heading 1) and categories (heading 2)
  (advice-add 'org-refile :after 'org-save-all-org-buffers) ; Automatically save after refiling

  ;; Org habit
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ) ; after! org ends

;; Shortcuts for agenda and capture
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; Org header outline using occur
(global-set-key (kbd "C-c o")
                (lambda()
                  "Display All Headings for an Org File Using Occur"
                  (interactive)
                  (occur "^*+ ")))

;; org-roam
(global-set-key (kbd "C-c f") #'org-roam-node-find)
(global-set-key (kbd "C-c i") #'org-roam-node-insert)
(global-set-key (kbd "C-c r b") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c r r") #'org-roam-db-sync)

;;; org-config.el ends here
