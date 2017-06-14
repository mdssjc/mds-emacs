;;; mds-aesthetic.el --- Estético (Aesthetic) -*- lexical-binding: t -*-
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
(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'prog-mode-hook    'global-hl-line-mode)
(add-hook 'prog-mode-hook    'nlinum-mode)

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (set-frame-font "Source Code Pro-10" nil t)
  (setq line-spacing 0.20
        nlinum-highlight-current-line t)
  (require 'mds-aesthetic-modeline))

(use-package nlinum-hl
  :ensure t
  :commands nlinum-hl-mode)

(use-package solaire-mode
  :ensure t
  :commands solaire-mode turn-on-solaire-mode solaire-mode-in-minibuffer
  :init
  (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode)
  (add-hook 'after-revert-hook            'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook        'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook    'solaire-mode))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5))
        dashboard-banner-logo-title "Welcome to MDS Emacs"
        dashboard-startup-banner dashboard-banner-logo-png)
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package highlight-thing
  :ensure t
  :diminish highlight-thing-mode
  :commands highlight-thing-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0)
  (set-face-attribute 'highlight-thing nil
                      :foreground "gold"
                      :background "#23272e"))

(use-package focus
  :ensure t
  :commands focus-mode
  :init
  (add-hook 'prog-mode-hook 'focus-mode))

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :commands git-gutter-mode
  :init
  (defun git-gutter-maybe ()
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook 'prog-mode-hook 'git-gutter-maybe)
  (add-hook 'text-mode-hook 'git-gutter-maybe)
  (add-hook 'org-mode-hook  'git-gutter-maybe))

(use-package git-gutter-fringe
  :ensure t
  :after git-gutter
  :config
  (setq git-gutter-fr:side 'right-fringe
        git-gutter:update-interval 0)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)
  (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)
  (add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
  (add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook))

(use-package emojify
  :ensure t
  :commands emojify-mode global-emojify-mode
  :init
  (setq emojify-emojis-dir (concat user-emacs-directory ".cache/emojis"))
  (add-hook 'text-mode-hook 'global-emojify-mode))

(use-package tabbar
  :ensure t
  :commands tabbar-mode
  :init
  (add-hook 'after-init-hook 'tabbar-mode)
  :config
  (setq tabbar-use-images nil))

(use-package tabbar-ruler
  :ensure t
  :after tabbar
  :config
  (setq tabbar-ruler-global-tabbar t
        tabbar-ruler-global-ruler t
        tabbar-ruler-popup-menu nil
        tabbar-ruler-popup-toolbar t
        tabbar-ruler-popup-scrollbar t))

(use-package golden-ratio
  :ensure t
  :diminish " φ"
  :commands golden-ratio-mode)

(use-package centered-cursor-mode
  :ensure t
  :diminish " ⊝"
  :commands centered-cursor-mode)

(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode)

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
