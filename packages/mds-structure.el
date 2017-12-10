;;; mds-structure.el --- Estrutura (Structure) -*- lexical-binding: t -*-
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
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end)) (message "Copied line")
       (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-save-file (expand-file-name (concat user-emacs-directory ".cache/recentf")))
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 15
        recentf-auto-cleanup 600
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-exclude '("/elpa/" "/.cache/")))

;; Saveplace
(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name (concat user-emacs-directory ".cache/places")))
  :config
  (setq save-place-forget-unreadable-files nil
        save-place-limit 500))

;; Savehist
(use-package savehist
  :ensure t
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-file (concat user-emacs-directory ".cache/savehist"))
  :config
  (setq history-length 250
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-autosave-interval 60
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-mode global-undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory ".cache/undo-tree"))))
  (add-hook 'after-init-hook 'global-undo-tree-mode)
  ;;(add-hook 'org-mode-hook  'undo-tree-mode)
  ;;(add-hook 'prog-mode-hook 'undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

(use-package magit
  :ensure t
  :ensure-system-package git
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-refine-hunk 'all))

(use-package gitignore-mode
  :ensure t
  :mode
  (("\\.gitignore$" . gitignore-mode)))

(use-package magithub
  :ensure t
  :after magit
  :config
  (setq magithub-cache t)
  (magithub-feature-autoinject t))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine-mode git-timemachine-toggle)

(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-timeout-seconds 0.3
        avy-all-windows nil
        avy-background t
        avy-case-fold-search nil)
  (avy-setup-default))

(use-package hydra
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 12
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-re-builders-alist '((read-file-name-internal . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-virtual-abbreviate 'full
        ivy-wrap t
        ivy-initial-inputs-alist nil))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (setq ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-xref
  :ensure t
  :after ivy
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package swiper
  :ensure t
  :commands swiper swiper-all
  :config
  (setq swiper-include-line-number-in-search t))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-at-point t
        confirm-nonexistent-file-or-buffer t
        enable-recursive-minibuffers t)
  (fset 'describe-function 'counsel-describe-function)
  (fset 'describe-variable 'counsel-describe-variable))

(use-package amx
  :ensure t
  :hook (after-init . amx-mode)
  :init
  (setq amx-save-file (concat user-emacs-directory ".cache/.amx-items"))
  :config
  (setq amx-auto-update-interval 10
        amx-history-length 5
        amx-backend 'ivy))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-cache-file (expand-file-name (concat user-emacs-directory ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory ".cache/projectile-bookmarks.eld")))
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-indexing-method 'alien
        projectile-enable-caching (not noninteractive)
        projectile-file-exists-remote-cache-expire nil
        projectile-file-exists-local-cache-expire (* 10 60)))

(use-package projectile-ripgrep
  :ensure t
  :commands projectile-ripgrep)

(use-package counsel-projectile
  :ensure t
  :hook (projectile-mode . counsel-projectile-on))

(use-package treemacs
  :ensure t
  :commands treemacs-toggle treemacs-projectile-toggle
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs     (if (executable-find "python") 3 0))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :ensure t
  :after treemacs
  :config
  (setq treemacs-header-function 'treemacs-projectile-create-header))

(use-package expand-region
  :ensure t
  :defer 1)

(use-package embrace
  :ensure t
  :commands embrace-commander
  :init
  (setq embrace-semantic-units-alist '((?w . er/mark-word)
                                       (?s . er/mark-symbol)
                                       (?S . er/mark-symbol-with-prefix)
                                       (?n . er/mark-next-accessor)
                                       (?m . er/mark-method-call)
                                       (?Q . er/mark-inside-quotes)
                                       (?q . er/mark-outside-quotes)
                                       (?P . er/mark-inside-pairs)
                                       (?p . er/mark-outside-pairs)
                                       (?c . er/mark-comment)
                                       (?u . er/mark-url)
                                       (?e . er/mark-email)
                                       (?d . er/mark-defun))))

(use-package ciel
  :ensure t
  :commands ciel-ci ciel-co ciel-copy-to-register)

(use-package iedit
  :ensure t
  :defer 0)

(use-package zop-to-char
  :ensure t
  :commands zop-to-char
  :init
  (global-set-key [remap zap-to-char] 'zop-to-char))

(use-package wgrep
  :ensure t
  :commands wgrep-setup
  :init
  (add-hook 'occur-mode-hook 'wgrep-setup)
  (add-hook 'rg-mode-hook    'wgrep-setup))

(use-package rg
  :ensure t
  :ensure-system-package rg
  :commands rg rg-project
  :init
  (rg-enable-default-bindings (kbd "s-/"))
  :config
  (setq rg-group-result t
        rg-show-columns t))

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp projectile-ripgrep)

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

(use-package winner
  :hook (after-init . winner-mode))

(use-package buffer-move
  :ensure t
  :commands buf-move-up buf-move-down buf-move-left buf-move-right)

(use-package move-dup
  :ensure t
  :commands md/move-lines-up md/move-lines-down md/duplicate-up md/duplicate-down)

(use-package electric-spacing
  :ensure t
  :diminish electric-spacing-mode
  :commands electric-spacing-mode)

(use-package which-func
  :hook (prog-mode . which-function-mode))

(use-package highlight
  :ensure t
  :diminish global-hi-lock-mode hi-lock-mode
  :commands global-hi-lock-mode
  :init
  (add-hook 'after-init-hook 'global-hi-lock-mode))

(use-package shift-number
  :ensure t
  :commands shift-number-up shift-number-down)

(use-package number
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :hook (after-init . multiple-cursors-mode))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :hook (after-init . global-page-break-lines-mode))

(use-package underline-with-char
  :ensure t
  :commands underline-with-char)

(use-package cmds-menu
  :ensure t
  :hook (after-init . recent-cmds-menu-mode))

(use-package icicles
  :ensure t
  :commands icy-mode)

(use-package doremi
  :ensure t
  :after icicles)

(use-package lacarte
  :ensure t
  :commands lacarte-execute-command)

(use-package package-utils
  :ensure t
  :commands package-utils-install-async package-utils-upgrade-all)

(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  (add-hook 'after-init-hook
            (lambda () (when (memq window-system '(mac ns x))
                    (exec-path-from-shell-initialize))))
  :config
  (setq exec-path-from-shell-check-startup-files nil))

(use-package symon
  :ensure t
  :commands symon-mode)

(use-package pandoc-mode
  :ensure t
  :bind
  (:map pandoc-mode-map
        ("C-c /" . nil)
        ("<C-M-return> a p" . pandoc-main-hydra/body))
  :commands pandoc-mode)

;; Plus

(use-package dired
  :defer 0
  :config
  (use-package dired+
    :ensure t
    :init
    (use-package tramp)
    :config
    (setq diredp-image-preview-in-tooltip t)))

(use-package bookmark
  :defer 0
  :config
  (use-package bookmark+
    :ensure t
    :init
    (defvaralias 'bmkp-replace-eww-keys-flag 'bmkp-replace-EWW-keys-flag)))

;;(eval-after-load "isearch" '(use-package isearch+ :ensure t :defer 0
;;                              :config
;;                              (isearchp-toggle-lazy-highlighting)))

;;(eval-after-load "isearch" '(use-package isearch-prop :ensure t :defer 0))

;; (use-package replace
;;   :defer 0
;;   :config
;;   (use-package replace+ :ensure t))
(use-package replace+
  :ensure t
  :defer 0
  :after replace)

;; (use-package menu-bar
;;   :defer 0
;;   :config
;;   (use-package menu-bar+ :ensure t))
(use-package menu-bar+
  :ensure t
  :defer 0
  :after menu-bar)

(use-package imenu
  :defer 0
  :config
  (setq imenu-auto-rescan t)
  (use-package imenu+
    :ensure t
    :config
    (eval-after-load 'imenu+
      `(add-to-list 'imenup-emacs-lisp-generic-expression
                    (list "Packages" ,use-package-form-regexp-eval 2))))
  (use-package popup-imenu :ensure t :commands popup-imenu))

(use-package face-remap
  :config
  (use-package face-remap+ :ensure t))

(use-package icomplete
  :config
  (use-package icomplete+ :ensure t))

(use-package pp
  :commands pp-eval-expression
  :config
  (use-package pp+
    :ensure t
    :config
    (global-set-key [remap eval-expression] 'pp-eval-expression)))
;; ---

(provide 'mds-structure)
;;; mds-structure.el ends here
