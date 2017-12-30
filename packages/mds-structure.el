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

;; Submodes
(electric-indent-mode -1)
;; ---

;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".cache/backups")))
      make-backup-files t
      backup-by-copying t
      version-control   t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9)

;; Autosave
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory ".cache/auto-save")))
      auto-save-list-file-prefix (concat user-emacs-directory ".cache/auto-save-list/.saves-")
      auto-save-default  t
      auto-save-timeout  60
      auto-save-interval 50)

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-save-file (expand-file-name (concat user-emacs-directory ".cache/recentf")))
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items  15
        recentf-auto-cleanup    600
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-exclude '("/elpa/"
                          "/.cache/"
                          "/usr/local/share/emacs/")))

;; Saveplace
(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (concat user-emacs-directory ".cache/places"))
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
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory ".cache/undo-tree"))))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

(use-package magit
  :ensure t
  :defer 0
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

(use-package ivy
  :ensure t
  :defer 0
  :bind
  (("M-X" . ivy-resume))
  :config
  (setq ivy-height 12
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-re-builders-alist '((read-file-name-internal . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-virtual-abbreviate 'full
        ivy-wrap t
        ivy-initial-inputs-alist nil)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package ivy-rich
  :ensure t
  :commands ivy-switch-buffer
  :config
  (setq ivy-rich-path-style 'abbrev))

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
  :defer 0
  :config
  (counsel-mode)
  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-at-point              t
        confirm-nonexistent-file-or-buffer      t
        enable-recursive-minibuffers            t)
  (defalias 'recentf 'counsel-recentf))

(use-package amx
  :ensure t
  :after counsel
  :config
  (setq amx-save-file (concat user-emacs-directory ".cache/.amx-items")
        amx-auto-update-interval 10
        amx-history-length 5
        amx-backend 'ivy)
  (defalias 'counsel-M-x 'amx))

(use-package projectile
  :ensure t
  :bind
  (:map projectile-mode-map
        ("<C-M-return> p"     . projectile-command-map)
        ("<C-M-return> p s R" . projectile-ripgrep)
        ("<C-M-return> p P"   . treemacs-projectile-toggle)
        ("C-c p"              . projectile-command-map)
        ("C-c p s R"          . projectile-ripgrep)
        ("C-c p P"            . treemacs-projectile-toggle)
        ("s-p"                . projectile-command-map)
        ("s-p s R"            . projectile-ripgrep)
        ("s-p P"              . treemacs-projectile-toggle)
        ("s-P"                . treemacs-projectile-toggle))
  :init
  (setq projectile-mode-line nil)
  :config
  (setq projectile-cache-file          (expand-file-name (concat user-emacs-directory ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
        projectile-completion-system 'ivy
        projectile-sort-order        'modification-time
        projectile-tags-backend      'xref
        projectile-file-exists-remote-cache-expire nil
        projectile-file-exists-local-cache-expire (* 10 60)
        projectile-mode-line nil)
  (projectile-mode)
  (counsel-projectile-mode)
  (which-key-add-prefix-title
    "<C-M-return> p"   "projectile"
    "<C-M-return> p 4" "find"
    "<C-M-return> p 5" "find other"
    "<C-M-return> p s" "search"
    "<C-M-return> p x" "execute"
    "C-c p"            "projectile"
    "C-c p 4"          "find"
    "C-c p 5"          "find other"
    "C-c p s"          "search"
    "C-c p x"          "execute"
    "s-p 4"            "find"
    "s-p 5"            "find other"
    "s-p s"            "search"
    "s-p x"            "execute"))

(use-package projectile-ripgrep
  :ensure t
  :commands projectile-ripgrep)

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-mode)

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
  (defalias 'zap-to-char 'zop-to-char))

(use-package wgrep
  :ensure t
  :hook (rg-mode . wgrep-setup))

(use-package rg
  :ensure t
  :commands rg-enable-default-bindings
  :bind-keymap
  ("s-/" . rg-global-map)
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (rg-enable-default-bindings "\C-x/")
              (which-key-add-prefix-title "C-x /" "rg")
              (general-define-key :keymaps 'projectile-mode-map
                                  "<C-M-return> p ." 'rg-project
                                  "C-c p ."          'rg-project
                                  "s-p ."            'rg-project)))
  :config
  (setq rg-group-result t
        rg-show-columns t
        rg-keymap-prefix "C-x /"))

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp projectile-ripgrep)

(use-package anzu
  :ensure t
  :commands global-anzu-mode anzu-query-replace anzu-query-replace-regexp anzu-replace-at-cursor-thing anzu-query-replace-at-cursor-thing
  :config
  (setq anzu-mode-lighter ""
        anzu-replace-to-string-separator " => "))

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
  :commands electric-spacing-mode)

(use-package which-func
  :hook (prog-mode . which-function-mode))

(use-package highlight
  :ensure t
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

(use-package auto-package-update
  :ensure t
  :commands auto-package-update-maybe
  :init
  (add-hook 'after-init-hook 'auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now")))
  (add-hook 'auto-package-update-after-hook
            (lambda () (message "Update completed successfully")))
  :config
  (setq auto-package-update-prompt-before-update t))

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

(use-package face-remap+
  :ensure t
  :after face-remap)

(use-package bookmark+
  :ensure t
  :after bookmark
  :init
  (defvaralias 'bmkp-replace-eww-keys-flag 'bmkp-replace-EWW-keys-flag))

(use-package imenu+
  :ensure t
  :after imenu
  :config
  (setq imenu-auto-rescan t)
  (eval-after-load 'imenu+
    `(add-to-list 'imenup-emacs-lisp-generic-expression
                  (list "Packages" ,use-package-form-regexp-eval 2)))
  (use-package popup-imenu :ensure t :commands popup-imenu))

(use-package icomplete+
  :ensure t
  :after icomplete)

(use-package menu-bar+
  :ensure t
  :after menu-bar)

(add-hook 'dired-mode-hook
          (lambda ()
            (use-package dired+
              :ensure t
              :config
              (use-package tramp)
              (setq diredp-image-preview-in-tooltip t))))

;; (use-package isearch+
;;   :ensure t
;;   :config
;;   (use-package isearch-prop :ensure t)
;;   (isearchp-toggle-lazy-highlighting))

;; (use-package replace+
;;   :ensure t
;;   :after replace)

(use-package pp+
  :ensure t
  :after pp
  :config
  (global-set-key [remap eval-expression] 'pp-eval-expression))
;; ---

(provide 'mds-structure)
;;; mds-structure.el ends here
