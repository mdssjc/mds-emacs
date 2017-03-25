;;; mds-aesthetic.el --- Estético (Aesthetic)
;;
;; Copyright (C) 2016-2017 Marcelo dos Santos
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
(use-package spaceline
  :ensure t
  :defer 0
  :config
  (require 'spaceline-config)
  (use-package spacemacs-theme :ensure t :defer 0
    :config
    (setq spacemacs-theme-org-highlight t))
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t
        Info-fontify-angle-bracketed-flag nil)
  (tabbar-mode)
  (global-hl-line-mode t)
  (set-frame-font "Source Code Pro-10" nil t)
  (spaceline-emacs-theme)
  (spaceline-info-mode)
  (load-theme 'spacemacs-dark t)
  (toggle-frame-maximized))

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5))
        dashboard-banner-logo-title "Welcome to MDS Emacs"))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package major-mode-icons
  :ensure t
  :diminish major-mode-icons-mode
  :commands major-mode-icons-mode
  :init
  (add-hook 'after-init-hook 'major-mode-icons-mode))

(use-package beacon
  :ensure t
  :commands beacon-mode
  :diminish beacon-mode
  :init
  (add-hook 'after-init-hook 'beacon-mode)
  :config
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 4
        beacon-color "#FF0000"))

(use-package emojify
  :ensure t
  :commands global-emojify-mode
  :init
  (add-hook 'after-init-hook 'global-emojify-mode)
  :config
  (setq emojify-emojis-dir (concat user-emacs-directory ".cache/emojis")))

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
