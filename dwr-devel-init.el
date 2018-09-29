;;; dwr-devel-init --- Emacs config for Development
;;; Commentary:

;;; Code:

;; -------------------------------------
;; General Development Stuff
;; -------------------------------------

;; Globally enable Company Mode (Text Completion)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(global-set-key (kbd "C-x g") 'magit-status)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; enable column numbers on Mode Line
(setq column-number-mode t)


;; -------------------------------------
;; Perl Development Stuff
;; -------------------------------------

(defalias 'perl-mode 'cperl-mode)

;; -------------------------------------
;; Docker Development Stuff
;; -------------------------------------

(require 'docker)
;;(docker-global-mode t)

;; -------------------------------------
;; Python Development Stuff
;; -------------------------------------

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

;; -------------------------------------
;; JavaScript Development Stuff
;; -------------------------------------

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

