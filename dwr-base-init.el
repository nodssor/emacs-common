;;; dwr-base-init --- Emacs basic config
;;; Commentary:

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq inhibit-startup-message t)

(setq inhibit-splash-screen t)

;; If running in X then set the frame size to something larger (more usable) than the default
(when window-system
  (set-frame-size (selected-frame) 200 60))

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
;(toggle-scroll-bar -1) ;; Disable scrollbars
(scroll-bar-mode 1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; Major Theme
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package doom-themes :defer t)
(use-package spacegray-theme :defer t)

::(load-theme 'doom-gruvbox t)
(load-theme 'darktooth t)
(doom-themes-visual-bell-config)

(use-package ace-window)

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
;(require 'helm)
;(helm-mode 1)(use-package command-log-mode)

;(global-set-key (kbd "M-x") 'helm-M-x)

(use-package magit)
;; Git setup - not just used in development :-)
(global-set-key (kbd "C-x g") 'magit-status)

;; Artist Mode stuff
(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)
   )

(use-package hydra)

(defhydra hydra-zoom (global-map "<f12>" :timeout 4)
  "Text zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(use-package swiper)

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

(ivy-mode 1)

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Improve Emacs Help System
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
