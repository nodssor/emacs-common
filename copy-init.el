(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode 't)

;;(require 'use-package)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") )
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
;;(add-to-list 'package-archives
;;             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(setq inhibit-splash-screen t)

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

;; Major Theme
(load-theme 'zenburn t)
;; Mode line theme (based on Powerline)
;; Requires Powerline fonts on Windows when using Putty
(require `airline-themes)
(load-theme 'airline-papercolor t)

;; Easy window navigation
(global-set-key (kbd "M-]") 'ace-window)

;; Setup basic Helm with M-x override
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Scroll window and keep point
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; Globally enable Company Mode (Text Completion)
(add-hook 'after-init-hook 'global-company-mode)
;; Enable Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defalias 'perl-mode 'cperl-mode)

(global-set-key (kbd "C-x g") 'magit-status)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; enable column numbers on Mode Line
(setq column-number-mode t)

(require 'ido)
(ido-mode t)

(require 'docker)
;;(docker-global-mode t)

(elpy-enable)

;; Set a more reasonable python debugger suggestion
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'pdb
      (file-name-nondirectory buffer-file-name)))))


(add-hook 'prog-mode-hook 'linum-mode)


;; *** Line Number Configuration block ****************
(require 'hlinum)
(hlinum-activate)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d\u2502")))
    ad-do-it))

;; ----------------------------
;; JavaScript Development
;; ---------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; For NodeJS scripting
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; NodeJS REPL
(require 'nodejs-repl)
;; JS Refactoring - https://github.com/magnars/js2-refactor.el
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")
(require 'flymake-jslint)
(add-hook 'js2-mode-hook 'flymake-jslint-load)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" default)))
 '(helm-mode t)
 '(jiralib-url "https://myjira.us.oracle.com")
 '(package-selected-packages
   (quote
    (org-jira helm zenburn-theme w3 use-package transpose-frame realgud pylint pallet nodejs-repl multishell micgoline markdown-preview-mode magit js2-refactor hlinum flymake-shell flymake-jslint flycheck elpy dockerfile-mode docker-api docker company-shell bash-completion auto-complete airline-themes ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
