;;; dwr-brain-init --- Emacs config for org-brain
;;; Commentary:
;;; Code:

;; Set up  org-brain
(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/.org-brain/brains")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
	  "* %i%?" :empty-lines 1)
	org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 21)
  (setq org-brain-include-file-entries nil
	org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;; Setup org-roam

;;; Tell Emacs where sqlite3.exe is stored
(add-to-list 'exec-path "/c/apps/sqlite-tools-win32-x86-3320200")
;;; Tell Emacs to start org-roam-mode when Emacs starts
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/org-roam")
(setq org-roam-db-location "~/org-roam/org-roam.db")

;;; ================================
;;; Define key bindings for Org-roam
;;; ================================
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display)
(global-set-key (kbd "C-c n i") #'org-roam-insert)
(global-set-key (kbd "C-c n /") #'org-roam-find-file)
(global-set-key (kbd "C-c n b") #'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n d") #'org-roam-find-directory)

;;; Recommendation for Windows users
(if (eq system-type 'windows-nt)
    (setq org-roam-db-update-method 'immediate)
    ;;; Let's also assign C-z to undo here
    (global-set-key (kbd "C-z") 'undo) ;Emacs default is bound to hide Emacs.
  )

