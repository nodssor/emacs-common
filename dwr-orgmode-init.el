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
(setq-default major-mode 'org-mode)
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
                         "~/gtd/work-projects.org"
			 "~/gtd/pers-projects.org"
			 "~/gtd/area.org"
                         "~/gtd/tickler.org"))

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
			       :Note:
			       :END:")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")

			      ("d" "Discussion Topic")
			      ("dp" "with Phani" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@phani:")
			      ("db" "with BrianS" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@brians:")
			      ("dt" "with Travis" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@travis:")
			      ("dm" "with Martin" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@martin:")
			      ("dw" "with Wanda" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@wanda:")
			      ("dl" "with Leadership Teams")
			      ("dlm" "with My Leadership Team" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@ross_leadership:")
			      ("dlp" "with Phani's Leadership Team" entry
			       (file+headline "~/gtd/work-discuss.org" "Discussions")
			       "* %?%i \t:@phani_leadership:")
			      ))
			       
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '(("~/gtd/prj-work.org" :maxlevel . 2)
			   ("~/gtd/prj-personal.org" :maxlevel . 2)
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
