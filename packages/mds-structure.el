;;; mds-structure.el --- Estrutura (Structure)
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
;; Conjunto estrutural de melhorias/funcionalidades para o ambiente Emacs.

;;; Code:
;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".cache/backups")))
      make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9)

;; Autosave
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory ".cache/auto-save")))
      auto-save-list-file-prefix (concat user-emacs-directory ".cache/auto-save-list/.saves-")
      auto-save-default t
      auto-save-timeout 60
      auto-save-interval 50)

(use-package recentf
  :commands recentf-mode
  :init
  (setq recentf-save-file (expand-file-name (concat user-emacs-directory ".cache/recentf")))
  (add-hook 'after-init-hook 'recentf-mode)
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 15
        recentf-auto-cleanup 600
        recentf-exclude '("/elpa/" "/.cache/")))

(use-package saveplace
  :commands save-place-mode
  :init
  (setq save-place-file (expand-file-name (concat user-emacs-directory ".cache/places")))
  (add-hook 'after-init-hook 'save-place-mode)
  :config
  (setq save-place-forget-unreadable-files nil
        save-place-limit 500))

(use-package savehist
  :ensure t
  :commands savehist-mode
  :init
  (setq savehist-file (concat user-emacs-directory ".cache/savehist"))
  (add-hook 'after-init-hook 'savehist-mode)
  :config
  (setq history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)))

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-mode global-undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory ".cache/undo-tree"))))
  (add-hook 'org-mode-hook  'undo-tree-mode)
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t))

(use-package magit
  :ensure t
  :defer 3)

(use-package magithub
  :load-path (lambda () (concat user-emacs-directory "temp/magithub"))
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine-mode git-timemachine-toggle)

(use-package git-gutter-fringe
  :ensure git-gutter
  :diminish git-gutter-mode
  :init
  (add-hook 'after-init-hook 'global-git-gutter-mode)
  :config
  (setq git-gutter-fr:side 'right-fringe
        git-gutter:update-interval 0)
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-foreground 'git-gutter-fr:deleted  "red"))

(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-timeout-seconds 0.3
        avy-background t
        avy-case-fold-search nil)
  (avy-setup-default))

(use-package hydra
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands ivy-mode
  :init
  (add-hook 'after-init-hook 'ivy-mode)
  :config
  (setq ivy-height 12
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-re-builders-alist '((read-file-name-internal . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-virtual-abbreviate 'full
        ivy-wrap t
        projectile-completion-system   'ivy
        magit-completing-read-function 'ivy-completing-read
        smex-completion-method         'ivy))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :ensure t
  :defer t
  :config
  (setq swiper-include-line-number-in-search t))

(use-package counsel
  :ensure t
  :defer t
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-at-point t))

(use-package smex
  :ensure t
  :after counsel
  :init
  (setq smex-save-file (concat user-emacs-directory ".cache/smex-items")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (expand-file-name (concat user-emacs-directory ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
        projectile-sort-order 'recentf
        projectile-enable-caching t
        projectile-file-exists-local-cache-expire (* 10 60)
        projectile-completion-system 'ivy)
  (projectile-mode)
  (counsel-projectile-on))

(use-package projectile-ripgrep
  :ensure t
  :commands projectile-ripgrep)

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-on)

(use-package projectile-speedbar
  :ensure t
  :commands projectile-speedbar-toggle projectile-speedbar-open-current-buffer-in-tree)

(use-package zop-to-char
  :ensure t
  :init
  (global-set-key [remap zap-to-char] 'zop-to-char))

(use-package expand-region
  :ensure t
  :defer 0
  :config
  (require 'the-org-mode-expansions))

(use-package embrace
  :ensure t
  :defer 0
  :init
  (setq semantics-units '((?w . er/mark-word)
                          (?s . er/mark-symbol)
                          (?d . er/mark-defun)
                          (?P . er/mark-inside-pairs)
                          (?p . er/mark-outside-pairs)
                          (?Q . er/mark-inside-quotes)
                          (?q . er/mark-outside-quotes)
                          (?. . er/mark-sentence)
                          (?h . er/mark-paragraph)
                          (?S . er/mark-symbol-with-prefix)
                          (?n . er/mark-next-accessor)
                          (?m . er/mark-method-call)
                          (?c . er/mark-comment)
                          (?u . er/mark-url)
                          (?e . er/mark-email)))
  (add-hook 'text-mode-hook
            '(lambda () (setq embrace-semantic-units-alist semantics-units)))
  (add-hook 'prog-mode-hook
            '(lambda () (setq embrace-semantic-units-alist semantics-units)))
  :config
  (require 'the-org-mode-expansions))

(use-package ciel
  :ensure t
  :commands ciel-ci ciel-co ciel-copy-to-register)

(use-package speedbar
  :defer t
  :commands speedbar
  :config
  (add-hook 'speedbar-after-create-hook  '(lambda () (global-set-key (kbd "C-c C-SPC") 'speedbar-get-focus)))
  (add-hook 'speedbar-before-delete-hook '(lambda () (global-unset-key (kbd "C-c C-SPC"))))
  (setq speedbar-show-unknown-files t
        speedbar-use-images nil))

(use-package rg
  :ensure t
  :commands rg)

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp projectile-ripgrep)

(use-package wgrep
  :ensure t
  :defer 0)

(use-package anzu
  :ensure t
  :commands global-anzu-mode anzu-query-replace anzu-query-replace-regexp anzu-replace-at-cursor-thing anzu-query-replace-at-cursor-thing
  :init
  (add-hook 'spaceline-pre-hook 'global-anzu-mode)
  :config
  (setq anzu-mode-lighter ""
        anzu-replace-to-string-separator " => "))

(use-package visual-regexp
  :ensure t
  :commands vr/replace vr/query-replace
  :config
  (use-package visual-regexp-steroids :ensure t))

(use-package eww
  :commands eww eww-mode
  :config
  (setq url-configuration-directory (concat user-emacs-directory ".cache/url")))

(use-package winner-mode
  :commands winner-mode
  :init
  (add-hook 'after-init-hook 'winner-mode))

(use-package move-dup
  :ensure t
  :commands md/move-lines-up md/move-lines-down md/duplicate-up md/duplicate-down)

(use-package swap-regions
  :ensure t
  :commands swap-regions-mode swap-regions
  :init
  (add-hook 'spaceline-pre-hook 'swap-regions-mode))

(use-package package-utils
  :ensure t
  :commands package-utils-install-async package-utils-upgrade-all)

(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  (add-hook 'after-init-hook '(lambda () (when (memq window-system '(mac ns x)))
                                      (exec-path-from-shell-initialize)))
  :config
  (setq exec-path-from-shell-check-startup-files nil))

(use-package electric-spacing
  :ensure t
  :diminish electric-spacing-mode
  :commands electric-spacing-mode)

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

(use-package emr
  :ensure t
  :commands emr-initialize emr-show-refactor-menu
  :init
  (add-hook 'prog-mode-hook 'emr-initialize))

(use-package dumb-jump
  :ensure t
  :commands dumb-jump-mode
  :init
  (add-hook 'prog-mode-hook 'dumb-jump-mode)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package which-func
  :commands which-function-mode
  :init
  (add-hook 'prog-mode-hook 'which-function-mode))

(use-package icicles
  :ensure t
  :commands icy-mode)

(use-package highlight
  :ensure t
  :diminish global-hi-lock-mode hi-lock-mode
  :commands global-hi-lock-mode
  :init
  (add-hook 'after-init-hook 'global-hi-lock-mode))

(use-package buffer-move
  :ensure t
  :commands buf-move-up buf-move-down buf-move-left buf-move-right)

(use-package symon
  :ensure t
  :commands symon-mode)

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :commands google-this-mode
  :init
  (add-hook 'after-init-hook 'google-this-mode))

(use-package google-translate
  :ensure t
  :after google-this
  :config
  (setq google-translate-show-phonetic t))

(use-package shift-number
  :ensure t
  :commands shift-number-up shift-number-down)

(use-package lacarte
  :ensure t
  :commands lacarte-execute-command)

(use-package multiple-cursors
  :ensure t
  :defer 0)

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :commands page-break-lines-mode)

(use-package popup-imenu
  :ensure t
  :commands popup-imenu)

;; Plus
(eval-after-load "dired" '(use-package dired+ :ensure t :defer 0
                            :init
                            (use-package tramp)
                            :config
                            (setq diredp-image-preview-in-tooltip t)))

(eval-after-load "mouse" '(use-package mouse+ :ensure t :defer 0))

(eval-after-load "info" '(use-package info+ :ensure t :defer 0))

(eval-after-load "bookmark" '(use-package bookmark+ :ensure t :defer 0
                               :init
                               (defvaralias 'bmkp-replace-eww-keys-flag 'bmkp-replace-EWW-keys-flag)))

(eval-after-load "isearch" '(use-package isearch+ :ensure t :defer 0
                              :config
                              (isearchp-toggle-lazy-highlighting)))

(eval-after-load "isearch" '(use-package isearch-prop :ensure t :defer 0))

(eval-after-load "replace" '(use-package replace+ :ensure t :defer 0))

(eval-after-load "menu-bar" '(use-package menu-bar+ :ensure t :defer 0))

(use-package imenu
  :init
  (add-hook 'prog-mode-hook 'imenu-add-menubar-index)
  (eval-after-load "imenu" '(use-package imenu+ :ensure t))
  :config
  (setq imenu-auto-rescan t))

(eval-after-load "face-remap" '(use-package face-remap+ :ensure t :defer 0))

(eval-after-load "icomplete" '(use-package icomplete+ :ensure t :defer 0))

(eval-after-load "pp" '(use-package pp+ :ensure t :defer 0))

(eval-after-load "simple" '(use-package simple+ :ensure t :defer 0))

(eval-after-load "hl-line" '(use-package hl-line+ :ensure t :defer 0))
;; ---

(provide 'mds-structure)
;;; mds-structure.el ends here
