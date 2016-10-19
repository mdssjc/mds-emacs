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

;;; Commentary:
;; Estilo ergonômico e sem distrações/ruídos - tema dark e linha de status com ícones.

;;; Code:
(use-package spacemacs-theme
  :ensure t
  :init
  (add-hook 'after-init-hook '(lambda () (load-theme 'spacemacs-dark t))))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (use-package info+
    :ensure t
    :config
    (with-eval-after-load 'info
      (require 'info+))
    (setq Info-fontify-angle-bracketed-flag nil))
  (setq spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (spaceline-emacs-theme)
  (spaceline-info-mode))

(use-package mode-icons
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'mode-icons-mode))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package beacon
  :ensure t
  :defer t
  :diminish beacon-mode
  :init
  (add-hook 'after-init-hook 'beacon-mode)
  :config
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 4
        beacon-color "#FF0000"))

(use-package emojify
  :ensure t
  :bind
  (("<f5> e" . global-emojify-mode))
  :init
  (add-hook 'after-init-hook 'global-emojify-mode) t)

(use-package volatile-highlights
  :ensure t
  :defer t
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
