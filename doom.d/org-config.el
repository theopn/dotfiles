(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/theo-org")

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

                                        ; Use Org-habit package
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; Org file locations
  (setq org-agenda-files
        '("capture.org"
          "education.org"
          "habits.org"
          "projects.org"
          "vita.org"
          "work.org"))

  ;; Org capture file
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry
            (file+headline org-default-notes-file "Tasks")
            "* IDEA %?\n  %i\n  %a" :empty-lines 1)
           ("j" "Journal"
            entry (file+datetree org-default-notes-file)
            "* %?\nEntered on %U\n  %i\n  %a")))

  ;; Archive can be acheived with C-c C-w
  (setq org-refile-targets ; Default current buffer + archive.org file
        '(("archive.org" :maxlevel . 2))) ; Detects upto heading 2, so make month (heading 1) and categories (heading 2)
  (advice-add 'org-refile :after 'org-save-all-org-buffers) ; Automatically save after refiling

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IDEA(i)" "|" "DONE(d)" "CANC(c)")))

  (setq org-agenda-span 8 ; Agenda shows 8 day
        org-deadline-warning-days 2
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d") ; Shows 3 days before

  ;; Configure custom agenda view with unspecified todo items from Org Capture (capture.org)
  (setq org-agenda-custom-commands
        '(("d" "Theo's Dashboard"
           ((agenda "")
            (todo "IDEA"
                  ((org-agenda-overriding-header "Captured Ideas/Tasks")
                   (org-agenda-files org-agenda-files)))
            ))))

  ) ; after! org ends

(global-set-key (kbd "C-c a") #'org-agenda)

;;; org-config.el ends here
