;;
;; Structure.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
;; Estrutura (Structure)
(require 'semantic)

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

;; Which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

;; Expand-Region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Magit
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
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus))))
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))
(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)))
;; Hydra

;; Projectile
;; TODO -> configurar o pacote
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode t))

(provide 'structure)
