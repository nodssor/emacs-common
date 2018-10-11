;;; dwr-base-init --- Emacs basic config
;;; Commentary:

;;; Code:

(setq inhibit-splash-screen t)

;; Major Theme
(load-theme 'zenburn t)
;; Mode line theme (based on Powerline)
;; Requires Powerline fonts on Windows when using Putty
(require `airline-themes)
(load-theme 'airline-papercolor t)

;; Easy window navigation
(global-set-key (kbd "M-]") 'ace-window)
(global-set-key (kbd "C-M-]") 'ace-swap-window)

;; Setup basic Helm with M-x override
(require 'helm)
;;(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Scroll window and keep point
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; Git setup - not just used in development :-)
(global-set-key (kbd "C-x g") 'magit-status)

