;;; dwr-orgmode-init --- Emacs config for Org Mode
;;; Commentary:

;;; Code:

;; Setup Org Mode and make the default mode
(require 'org)
(require 'org-bullets)
(require 'org-colored-text) ; Allows for colored text
(require 'ox-md)
(require 'ox-jira)

(add-to-list `auto-mode-alist '("\\.org$" . org-mode))
;(setq-default major-mode 'org-mode)
(add-hook 'org-mode-hook #'visual-line-mode)  ; Wrap long lines at window edge
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))) ; Use prettier bullets
(setq org-log-into-drawer 'LOGBOOK) ; Put notes into LOGBOOK Drawer
(setq org-hide-emphasis-markers 't) ; Hide markers for bold, italics, etc.
(setq org-use-sub-superscripts '{}) ; Allows underline chars to be used
(setq org-export-with-sub-superscripts '{}) ; Allows underline chars to be used

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key (kbd "C-c C-.") 'org-time-stamp-inactive)


(setq org-log-done 'time)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/projects.org"
			 "~/gtd/area.org"
			 "~/gtd/discuss.org"
			 "~/gtd/meetings.org"
			 "~/gtd/x-area.org"
			 "~/gtd/x-projects.org"
			 "~/gtd/skylander.org"
                         "~/gtd/tickler.org"))

; Set registers for quick loading/jumping to org files
; Using C-x r j <letter>
(set-register ?i '(file . "~/gtd/inbox.org"))
(set-register ?p '(file . "~/gtd/projects.org"))
(set-register ?a '(file . "~/gtd/area.org"))
(set-register ?d '(file . "~/gtd/discuss.org"))
(set-register ?m '(file . "~/gtd/meetings.org"))
(set-register ?s '(file . "~/gtd/skylander.org"))
(set-register ?r '(file . "~/gtd/resources.org"))
; Also allow a quick jump to the orgmode configuration (this file)
(set-register ?O '(file . "~/.emacs.d/emacs-common/dwr-orgmode-init.el"))


(setq org-todo-keywords '((sequence
			   "TODO(t)"
			   "WORKING(s!)"
			   "WAITING(w@/!)"
			   "|" "DONE(x!)" "DELEGATED(-@/!)")
			  ))

(setq org-todo-keyword-faces
      '(("TODO" . "sienna2")
        ("WORKING" ."mediumseagreen")
        ("DONE" . "darkcyan")
        ("WAITING" . "goldenrod3")
        ("DELEGATED" . "antiquewhite4")
       ))

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
                               :URL:
			       :Note:
			       :END:")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")
			      ("m" "Meeting" entry
			       (file+datetree "~/gtd/meetings.org")
			       "* %^{Meeting Title}\n  :WHEN: %U\n  :WHO: %^{Attendees}\n** Notes\n*** %?" :tree-type 'week)

			      ("d" "Discussion Topic")
			      ("dp" "with Phani" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@phani:")
			      ("db" "with BrianS" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@brians:")
			      ("dt" "with Travis" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@travis:")
			      ("dm" "with Micah" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@micah:")
			      ("dj" "with Jillian" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@jillian:")
			      ("dM" "with Martin" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@martin:")
			      ("dw" "with Wanda" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@wanda:")
			      ("dl" "with My Leadership Team" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@my_leaders:")
			      ("dL" "with Phani's Leadership Team" entry
			       (file+headline "~/gtd/discuss.org" "Discussions")
			       "* %?%i \t:@sce_leaders:")
			      ))
			       
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '(("~/gtd/projects.org" :maxlevel . 2)
			   ("~/gtd/x-projects.org" :maxlevel . 2)
			   ("~/gtd/area.org" :maxlevel . 3)
			   ("~/gtd/x-area.org" :maxlevel . 3)
			   ("~/gtd/discuss.org" :maxlevel . 2)
			   ("~/gtd/skylander.org" :maxlevel . 3)
			   ("~/gtd/archives.org" :maxlevel . 2)
			   ("~/gtd/someday.org" :maxlevel . 1)
			   ("~/gtd/resources.org" :maxlevel . 2)
			   ("~/gtd/tickler.org" :maxlevel . 2)))
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling
(setq org-completion-use-ido t)

(setq org-agenda-custom-commands
      
      '(("n" "All Next Tasks" tags-todo "+active"
         ((org-agenda-overriding-header "Active Project Next Tasks")
	  (org-agenda-files '("~/gtd/projects.org"))
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))

	("q" "Weekly Heads Up Display"
	 ((agenda "" ((org-agenda-span 7))
		  ((org-agenda-overriding-header "Time Sensitive Tasks")))

	  ; Anything still left in Inbox (Level 2 items with tags starting with t,p,r)
	  ; Which is everything
	  (tags "LEVEL=2+{^[tpr].*}"
		((org-agenda-overriding-header "Needs Filing")
		 (org-agenda-files '("~/gtd/inbox.org"))))

	  ; Loose tasks not associated with a project
	  (todo "TODO|WORKING"
		((org-agenda-overriding-header "Individual Tasks")
		 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
		 (org-agenda-files '("~/gtd/meetings.org" "~/gtd/area.org" "~/gtd/x-area.org"))))

	  ; Next steps on all active projects
	  (tags-todo "+active"
		     ((org-agenda-overriding-header "Active Project Next Tasks")
		      (org-agenda-files '("~/gtd/projects.org"))
		      (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
	  
	  (todo "WAITING|DELEGATED"
		((org-agenda-overriding-header "Delegated or Waiting On Tasks")
		 (org-agenda-files '("~/gtd/projects.org" "~/gtd/area.org"))
		 ))

	  ; Next steps on all active projects
	  (tags-todo "+Don"
		     ((org-agenda-overriding-header "Skylander Media Tasks")
		      (org-agenda-files '("~/gtd/skylander.org"))
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
	  ))

	;; Completed tasks for status reports
	("w" "Weekly Review"
	 agenda ""
	 ((org-agenda-start-day "-7d")
	  (org-agenda-overriding-header "The Week In Review")
	  (org-agenda-span 14)
	  (org-agenda-start-on-weekday 1)
	  (org-agenda-start-with-log-mode `(closed state))
	  ;(org-agenda-skip-function `(org-agenda-skip-entry-if `notregexp "^\\*+ DONE "))
	  (org-agenda-files '("~/gtd/projects.org" "~/gtd/area.org" "~/gtd/meetings.org"))
	  ))

	("d" . "Discussion Topics")
	("dp" "To Discuss with Phani" tags "@phani"
	 ((org-agenda-overriding-header "Discussion Points with Phani")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("db" "To Discuss with Brian" tags "@brian"
	 ((org-agenda-overriding-header "Discussion Points with Brian")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dt" "To Discuss with Travis" tags "@travis"
	 ((org-agenda-overriding-header "Discussion Points with Travis")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dm" "To Discuss with Micah" tags "@micah"
	 ((org-agenda-overriding-header "Discussion Points with Micah")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dj" "To Discuss with Jillian" tags "@jillian"
	 ((org-agenda-overriding-header "Discussion Points with Jillian")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dM" "To Discuss with Martin" tags "@martin"
	 ((org-agenda-overriding-header "Discussion Points with Martin")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dw" "To Discuss with Wanda" tags "@wanda"
	 ((org-agenda-overriding-header "Discussion Points with Wanda")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dL" "To Discuss with SCE Leadership" tags "@sce_leaders"
	 ((org-agenda-overriding-header "Discussion Points with SCE Leadership")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	("dl" "To Discuss with My Leadership" tags "@my_leaders"
	 ((org-agenda-overriding-header "Discussion Points with My Leadership")
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
	))

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

;; Org Babel Setup
;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (plantuml . t)
   ))
(setq org-plantuml-jar-path
      (expand-file-name "~/local/lib/plantuml.jar"))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-image-actual-width nil)

;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
