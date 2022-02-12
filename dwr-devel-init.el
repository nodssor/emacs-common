;;; dwr-devel-init --- Emacs config for Development
;;; Commentary:

;;; Code:

;; -------------------------------------
;; General Development Stuff
;; -------------------------------------

(use-package yasnippet)
(yas-global-mode 1)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c d")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package treemacs
  :ensure t
  :defer t)

;; Enable Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; default to unified diffs
(setq diff-switches "-u")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :bind ("C-c =" . hs-hide-all)
  :bind ("C-c +" . hs-show-all)
  :commands hs-toggle-hiding
  :defer t)

;; -------------------------------------
;; Markdown / GFM Editing
;; -------------------------------------

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc -c file:///home/don/.emacs.d/emacs-common/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

;; -------------------------------------
;; AsciiDoc Editing
;; -------------------------------------

(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;; Python3
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))
