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

(defun my-org-mode-config ()
  "For use in `org-mode-hook'."
  (local-set-key (kbd "S-<right>") 'org-todo)
  (setq org-todo-keywords '((sequence "TODO" "WORKING" "WAITING" "|" "DONE" "DELEGATED")))
  
)
;; add to hook
(add-hook 'org-mode-hook 'my-org-mode-config)

