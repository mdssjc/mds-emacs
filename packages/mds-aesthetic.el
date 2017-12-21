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
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (require 'mds-aesthetic-modeline)
  (cond ((member "Hack" (font-family-list))
         (progn
           (set-face-attribute 'default nil            :font "Hack 12")
           (set-face-attribute 'mode-line nil          :font "Hack 10")
           (set-face-attribute 'mode-line-inactive nil :font "Hack 10")))
        ((member "Source Code Pro" (font-family-list))
         (progn
           (set-face-attribute 'default nil            :font "Source Code Pro-12")
           (set-face-attribute 'mode-line nil          :font "Source Code Pro-10")
           (set-face-attribute 'mode-line-inactive nil :font "Source Code Pro-10")))
        ((member "DejaVu Sans Mono" (font-family-list))
         (progn
           (set-face-attribute 'default nil            :font "DejaVu Sans Mono 12")
           (set-face-attribute 'mode-line nil          :font "DejaVu Sans Mono 10")
           (set-face-attribute 'mode-line-inactive nil :font "DejaVu Sans Mono 10"))))
  (set-face-attribute 'line-number-current-line nil
                      :weight 'bold
                      :foreground "white")
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (setq Info-fontify-angle-bracketed-flag nil
        doom-themes-enable-bold   t
        doom-themes-enable-italic t
        line-spacing 0.10)
  (setq-default fringes-outside-margins t
                indicate-buffer-boundaries 'right
                indicate-empty-lines       'indicate-buffer-boundaries)
  ;; Hooks:
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode)
  (add-hook 'after-revert-hook            'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook        'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook    'solaire-mode))

(use-package tao-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'tao-yang t))

(use-package hl-line
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag        nil
        global-hl-line-sticky-flag nil))

(use-package solaire-mode
  :ensure t
  :commands solaire-mode turn-on-solaire-mode solaire-mode-in-minibuffer)

(use-package dashboard
  :ensure t
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 10)
                          (registers . 5))
        dashboard-banner-logo-title "Welcome to MDS Emacs"
        dashboard-startup-banner dashboard-banner-logo-png))

(use-package all-the-icons
  :ensure t)

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
  (setq highlight-thing-delay-seconds 0
        highlight-thing-exclude-thing-under-point t
        highlight-thing-case-sensitive-p t)
  (set-face-attribute 'highlight-thing nil
                      :foreground "gold"
                      :background "#23272e"))

(use-package focus
  :ensure t
  :hook (prog-mode . focus-mode))

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode))

(use-package git-gutter-fringe
  :ensure t
  :commands global-git-gutter-mode
  :init
  (add-hook 'after-init-hook 'global-git-gutter-mode)
  :config
  (require 'git-gutter)
  (setq git-gutter-fr:side 'left-fringe
        git-gutter:separator-sign "|"
        git-gutter:lighter "")
  (set-face-foreground 'git-gutter:separator "yellow")
  (fringe-helper-define 'git-gutter-fr:modified nil
                        "X"
                        "X"
                        "X"
                        "X"
                        "X"
                        "X"
                        "X"
                        "X"))

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

(use-package indent-info
  :ensure t
  :commands global-indent-info-mode
  :init
  (add-hook 'after-init-hook 'global-indent-info-mode)
  :config
  (setq indent-info-space-format "SPC[%s]"
        indent-info-tab-format   "TAB[%s]"))

(use-package total-lines
  :ensure t
  :hook (after-init . global-total-lines-mode))

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
