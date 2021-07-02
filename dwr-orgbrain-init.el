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

