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
;; Estilo ergonômico e sem distrações/ruídos - tema dark, linha de status com ícones.

;;; Code:
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package spaceline
  :ensure t
  :after spacemacs-theme
  :init
  (require 'spaceline-config)
  :config
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
  :commands mode-icons-mode
  :config
  (mode-icons-mode 1))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 4
        beacon-color "#FF0000")
  (beacon-mode 1))

(use-package emojify
  :ensure t
  :bind
  (("<f5> e" . global-emojify-mode))
  :init
  (add-hook 'after-init-hook 'global-emojify-mode) t)

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
