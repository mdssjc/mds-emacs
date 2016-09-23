;;; mds-structure.el --- Estrutura (Structure)
;;
;; Copyright (C) 2016-2016 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Configurações e melhorias ao editor

;;; Code:
(require 'semantic)

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package magit
  :ensure t
  :bind
  (("C-x g s" . magit-status)
   ("C-x g S" . magit-stage-file)
   ("C-x g g" . magit-dispatch-popup)))

;; Abo-abo (https://github.com/abo-abo)
(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char-timer)
  :config (setq avy-timeout-seconds 0.3))
(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :config (setq aw-dispatch-always t))
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus))))
(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)))
;; Hydra

(use-package counsel-projectile
  :ensure t)
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (counsel-projectile-on))

(provide 'mds-structure)
;;; mds-structure.el ends here
