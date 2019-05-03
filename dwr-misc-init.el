;;; dwr-misc-init --- Emacs config for miscellaneous stuff
;;; Commentary:
;;; Code:

;; Set up Ispell
(setq ispell-dictionary "english")
(setq ispell-personal-dictionary (dwr-get-fullpath ".aspell.en.pws"))
(global-set-key (kbd "C-c i") 'ispell-buffer)

