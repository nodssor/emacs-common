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

;; default to unified diffs
(setq diff-switches "-u")

;; enable column numbers on Mode Line
;;(setq column-number-mode t)

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :bind ("C-c =" . hs-hide-all)
  :bind ("C-c +" . hs-show-all)
  :commands hs-toggle-hiding
  :defer t)

;; -------------------------------------
;; Perl Development Stuff
;; -------------------------------------

(use-package elpy)
(defalias 'perl-mode 'cperl-mode)

;; -------------------------------------
;; Python Development Stuff
;; -------------------------------------

;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")
(add-hook 'elpy-mode-hook 'hs-minor-mode)      
(elpy-enable)

;; Set a more reasonable python debugger suggestion
(setq pdb-path '/usr/lib/python3.8/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
      (file-name-nondirectory buffer-file-name)))))


;; -------------------------------------
;; Markdown / GFM Editing
;; -------------------------------------

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc -c file:///home/don/.emacs.d/emacs-common/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

;; -------------------------------------
;; AsciiDoc Editing
;; -------------------------------------

(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
