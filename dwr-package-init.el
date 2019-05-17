;;; dwr-misc-init --- Emacs config for package management
;;; Commentary:

;;; Code:

(require 'cask "~/.emacs.d/emacs-common/cask/cask.el")
(cask-initialize "~/.emacs.d/emacs-common")

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


