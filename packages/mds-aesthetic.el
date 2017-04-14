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
(set-frame-font "Source Code Pro-10" nil t)
(setq line-spacing 0.15)

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'global-hl-line-mode)
(add-hook 'prog-mode-hook 'color-identifiers-mode)

(use-package tao-theme
  :ensure t
  :defer 0
  :init
  (load-theme 'tao-yin t)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (tabbar-mode))
  ;(fringe-mode '(16 . 16)))

(use-package color-identifiers-mode
  :ensure t
  :diminish color-identifiers-mode
  :commands color-identifiers-mode)

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5))
        dashboard-banner-logo-title "Welcome to MDS Emacs"
        dashboard-startup-banner dashboard-banner-logo-png))

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

(use-package focus
  :ensure t
  :commands focus-mode
  :init
  (add-hook 'prog-mode-hook 'focus-mode))

(use-package highlight-thing
  :ensure t
  :diminish highlight-thing-mode
  :commands highlight-thing-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0)
  (set-face-attribute 'highlight-thing nil
                      :weight 'bold
                      :foreground "gold1"
                      :background "black"))

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

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(use-package emojify
  :ensure t
  :commands global-emojify-mode
  :init
  (setq emojify-emojis-dir (concat user-emacs-directory ".cache/emojis"))
  (add-hook 'after-init-hook 'global-emojify-mode))

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
