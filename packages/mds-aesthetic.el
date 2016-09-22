;;; mds-aesthetic.el --- Estético (Aesthetic)
;;
;; Copyright (C) 2016-2016 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense
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

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
