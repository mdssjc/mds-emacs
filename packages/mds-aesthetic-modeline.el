;;; mds-aesthetic-modeline.el --- Estético - Modeline (Modeline - Aesthetic) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Complemento do Estético com as configurações do Modeline.

;;; Code:
(use-package powerline
  :ensure t
  :defer 0
  :config
  (setq powerline-default-separator 'slant
        powerline-height 14
        powerline-default-separator-dir '(right . right))
  (display-time-mode))

(use-package spaceline
  :ensure t
  :after powerline
  :config
  (require 'spaceline-config)
  (spaceline-info-mode))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  (setq spaceline-all-the-icons-separator-type 'none
        spaceline-all-the-icons-icon-set-git-stats 'arrows)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-all-the-icons-theme)
  (set-face-attribute 'mode-line nil :font "Source Code Pro-10"))

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
