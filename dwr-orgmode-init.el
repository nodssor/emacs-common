;;; dwr-orgmode-init --- Emacs config for Org Mode
;;; Commentary:

;;; Code:

;; Setup Org Mode and make the default mode
(require 'org)
(add-to-list `auto-mode-alist '("\\.org$" . org-mode))
(setq-default major-mode 'org-mode)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done 'time)
(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/projects.org"
			 "~/gtd/area.org"
                         "~/gtd/tickler.org"))
(setq org-todo-keywords '((sequence "TODO" "WORKING" "WAITING" "|" "DONE" "DELEGATED")))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("p" "Project [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Potential Projects")
			       "* %i%?")
			      ("r" "Resource" entry
			       (file+headline "~/gtd/inbox.org" "Resources")
			       "* %i%?
			       :PROPERTIES:
                               :Area:
			       :Note:
			       :END:")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '(("~/gtd/projects.org" :maxlevel . 2)
			   ("~/gtd/area.org" :maxlevel . 3)
			   ("~/gtd/archives.org" :maxlevel . 2)
			   ("~/gtd/someday.org" :maxlevel . 1)
			   ("~/gtd/resources.org" :maxlevel . 2)
			   ("~/gtd/tickler.org" :maxlevel . 2)))
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling
(setq org-completion-use-ido t)

(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "office"
         ((org-agenda-overriding-header "Office")))


	("n" "All Next Tasks" todo ""
         ((org-agenda-overriding-header "Next")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (or (string= "TODO" (org-get-todo-state))
      (string= "WORKING" (org-get-todo-state))))

