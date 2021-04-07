;;; dwr-devel-go-init --- Emacs config for Go Development
;;; Commentary:

;;; Code:

;; -------------------------------------
;; Go Development Stuff
;; -------------------------------------

(defun go-mode-setup ()
  (setq tab-width 4 indent-tabs-mode 1)
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (setq compile-command "go build -v -o build.out && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (auto-complete-mode 1)
  (company-mode)
  (set (make-local-variable 'company-backends) '(company-go))
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test)
    (define-key map (kbd "C-c b") 'go-run))
)
 
(add-hook 'go-mode-hook 'go-mode-setup)
(add-hook 'go-mode-hook 'hs-minor-mode)

(require 'auto-complete-config)
;;(require 'go-autocomplete)


