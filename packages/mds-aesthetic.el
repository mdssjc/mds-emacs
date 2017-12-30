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
  (add-hook 'prog-mode-hook               'display-line-numbers-mode)
  (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode)
  (add-hook 'after-revert-hook            'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook        'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook    'solaire-mode))

(use-package tao-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'tao-yang t))

(use-package dashboard
  :ensure t
  :if (< (length command-line-args) 2)
  :config
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5)
                          (registers . 5)
                          (agenda    . 10))
        dashboard-banner-logo-title "Welcome to MDS Emacs"
        dashboard-startup-banner     dashboard-banner-logo-png)
  (add-hook 'after-init-hook
            (lambda ()
              (dashboard-insert-startupify-lists)
              (kill-matching-buffers "\\.org" nil t)
              (switch-to-buffer "*dashboard*")
              (goto-char (point-min))
              (redisplay))))

(use-package hl-line
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(use-package solaire-mode
  :ensure t
  :commands solaire-mode turn-on-solaire-mode solaire-mode-in-minibuffer)

(use-package all-the-icons
  :ensure t
  :defer  0)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package highlight-thing
  :ensure t
  :hook (prog-mode . highlight-thing-mode)
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
  :hook (after-init . volatile-highlights-mode))

(use-package git-gutter-fringe
  :ensure t
  :hook (after-init . global-git-gutter-mode)
  :config
  (require 'git-gutter)
  (setq git-gutter-fr:side 'left-fringe
        git-gutter:separator-sign "|"
        git-gutter:lighter ""
        git-gutter:update-interval 1)
  (set-face-foreground  'git-gutter:separator "yellow")
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
  :hook (text-mode . emojify-mode)
  :config
  (setq emojify-emojis-dir (concat user-emacs-directory ".cache/emojis")))

(use-package tabbar
  :ensure t
  :hook (after-init . tabbar-mode)
  :config
  (setq tabbar-use-images nil))

(use-package tabbar-ruler
  :ensure t
  :after tabbar
  :config
  (setq tabbar-ruler-global-tabbar   t
        tabbar-ruler-global-ruler    t
        tabbar-ruler-popup-menu      nil
        tabbar-ruler-popup-toolbar   t
        tabbar-ruler-popup-scrollbar t
        tabbar-ruler-use-mode-icons  nil))

(use-package golden-ratio
  :ensure t
  ;; :diminish "φ"
  :commands golden-ratio-mode
  :config
  (setq golden-ratio-))

(use-package centered-cursor-mode
  :ensure t
  ;; :diminish "¢"
  :commands centered-cursor-mode)

(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode)

(use-package indent-info
  :ensure t
  :hook (after-init . global-indent-info-mode)
  :bind
  (:map indent-info-mode-map
        ("C-M-~" . nil)
        ("C-M->" . nil)
        ("C-M-<" . nil))
  :config
  (setq indent-info-space-format "⌧(%s)"
        indent-info-tab-format   "⭾(%s)"
        indent-info-suffix       ""))

(use-package fill-column-indicator
  :ensure t
  :hook (prog-mode . fci-mode)
  :config
  (setq fci-rule-color "#3f444a"))

(provide 'mds-aesthetic)
;;; mds-aesthetic.el ends here
