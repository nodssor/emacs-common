;;; dwr-base-init --- Emacs basic config
;;; Commentary:

;;; Code:

(setq inhibit-splash-screen t)


;; Determine the specific system type. Specifcally setting up WSL specifics.
;; Emacs variable system-type doesn't yet have a "wsl/linux" value,
;; so I'm front-ending system-type with my variable: system-type-specific
(defvar system-type-specific)
(setq-default system-type-specific  system-type) ;; get the system-type value

(cond
 ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
 ((eq system-type-specific 'gnu/linux)
  (when (string-match "Linux.*Microsoft.*Linux"
                      (shell-command-to-string "uname -a"))

    (setq-default system-type-specific "wsl/linux") ;; for later use.
    ;; Below allows emacs to lauch native Windows default browser.
    (defvar wsl-cmd-exe)
    (defvar wsl-cmd-exe-args)
    (setq
     wsl-cmd-exe "/mnt/c/Windows/System32/cmd.exe"
     wsl-cmd-exe-args '("/c" "start" "") )
    (setq
     browse-url-generic-program  wsl-cmd-exe
     browse-url-generic-args     wsl-cmd-exe-args
     browse-url-browser-function 'browse-url-generic)
    )))

;; GUI Adjustments
(tool-bar-mode -1) ;; Disable toolbar
(toggle-scroll-bar -1) ;; Disable scrollbars


;; Major Theme
(load-theme 'zenburn t)
;; Mode line theme (based on Powerline)
;; Requires Powerline fonts on Windows when using Putty
(require `airline-themes)
(load-theme 'airline-papercolor t)

;; Movement and Navigation
;; Easy window navigation
(global-set-key (kbd "M-]") 'ace-window)
(global-set-key (kbd "C-M-]") 'ace-swap-window)

;; Move line
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-[") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; Scroll behavior
;; Scroll window and keep point
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")
(setq scroll-conservatively 5)
(setq scroll-preserve-screen-position t)


;; Global Format and Display
;; Truncate lines by default and set key binding
(set-default 'truncate-lines t)
(global-set-key (kbd "C-x \\") 'toggle-truncate-lines)


;; Setup basic Helm with M-x override
(require 'helm)
;;(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Git setup - not just used in development :-)
(global-set-key (kbd "C-x g") 'magit-status)

;; Artist Mode stuff
(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)
)
