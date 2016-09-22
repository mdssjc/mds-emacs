;;
;; Aesthetic.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
;; Estético (Aesthetic)
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))
(use-package spaceline
  :ensure t
  :after spacemacs-theme
  :init (require 'spaceline-config)
  :config (spaceline-emacs-theme))
(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))
