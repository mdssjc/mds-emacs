;;; mds-structure.el --- Estrutura (Structure)
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
;; Conjunto estrutural de melhorias/funcionalidades para o ambiente:
;;  - saveplace: salva a última posição do buffer;
;;  - recentf: listagem dos buffers mais recentes;
;;  - restart-emacs: reinicialização do ambiente;
;;  - undo-tree: visualização da árvore de modificações do buffer;
;;  - keyfreq: sumariza a frequência de utilização dos atalhos;
;;  - neotree: visualização do sistema de arquivos;
;;  - magit: controle de versão pelo Git com Time Machine, Fringe e Hub;
;;  - pacotes Abo-abo: Avy, Hydra, Ace-Window, Ivy, Swiper e Counsel;
;;  - smex: histórico de comandos;
;;  - expand-region: seleção de regiões;
;;  - embrace: manipulação de símbolos entre seleção;
;;  - selected: funcionalidades com a seleção ativa;
;;  - projectile: navegador de projetos;
;;  - ripgrep: ferramenta de busca Ripgrep (rg);
;;  - eww: browser interno;
;;  - popup-imenu: popup do imenu;
;;  - move-dup: move e/ou duplica linhas;
;;  - swap-regions: intercambia regiões;
;;  - tabbar: abas com os buffers;
;;  - package-utils: atualização automática e assíncrona dos pacotes;
;;  - exec-path-from-shell: variáveis ambiente do shell;
;;  - dashboard: quadro com o resumo do ambiente;
;;  - hl-todo: highlight dos labels de todo;
;;  - electric-spacing: espaço entre operadores, conforme modo maior;
;;  - golden-ratio: taxa de redimensionamento para o buffer ativo;
;;  - centered-cursor: manter o cursor no centro do buffer;
;;  - writeroom: ambiente sem distração visual;
;;  - focus: prioriza a região de edição ativa;
;;  - plus: dired+, replace+, mouse+, menu-bar+, info+, isearch+ e bookmark+.

;;; Code:
(use-package saveplace
  :commands save-place-mode
  :init
  (setq save-place-file (expand-file-name (concat user-emacs-directory ".cache/places")))
  (add-hook 'after-init-hook 'save-place-mode)
  :config
  (setq save-place-forget-unreadable-files nil))

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

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :init
  (add-hook 'after-init-hook 'global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package keyfreq
  :ensure t
  :commands keyfreq-show
  :init
  (add-hook 'after-init-hook '(lambda ()
                                (keyfreq-mode 1)
                                (keyfreq-autosave-mode 1)))
  (setq keyfreq-file      (concat user-emacs-directory ".cache/.emacs.keyfreq")
        keyfreq-file-lock (concat user-emacs-directory ".cache/.emacs.keyfreq.lock")))

(use-package neotree
  :ensure t
  :commands neotree-toggle
  :config
  (setq neo-theme (if window-system 'icons 'nerd)
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-keymap-style 'concise))

(use-package magit
  :ensure t
  :commands magit-status)

(use-package git-timemachine
  :ensure t
  :after magit)

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (add-hook 'after-init-hook 'global-git-gutter-mode)
  :config
  (setq git-gutter-fr:side 'right-fringe
        git-gutter:update-interval 5))

(add-to-list 'load-path (concat user-emacs-directory "temp/magithub"))
(use-package magithub
  :after magit
  :config
  (require 'magithub))

(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :config
  (setq avy-timeout-seconds 0.3
        avy-background t))

(use-package hydra
  :ensure t)

(use-package ace-window
  :ensure t
  :commands ace-window
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist '((?x aw-delete-window     "Ace - Delete Window")
                            (?c aw-swap-window       "Ace - Swap Window")
                            (?n aw-flip-window)
                            (?v aw-split-window-vert "Ace - Split Vert Window")
                            (?h aw-split-window-horz "Ace - Split Horz Window")
                            (?m delete-other-windows "Ace - Maximize Window")
                            (?g delete-other-windows)
                            (?b balance-windows)
                            (?u winner-undo)
                            (?r winner-redo))
        aw-background t)
  (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
  (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
  (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)
  (ace-window-display-mode t))

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
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-virtual-abbreviate 'full
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
  :defer t)

(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (concat user-emacs-directory ".cache/smex-items")))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :config
  (require 'the-org-mode-expansions))

(use-package embrace
  :ensure t
  :init
  (add-hook 'text-mode-hook '(lambda () (setq embrace-semantic-units-alist
                                         (append embrace-semantic-units-alist semantics-units))))
  (add-hook 'prog-mode-hook '(lambda () (setq embrace-semantic-units-alist
                                         (append embrace-semantic-units-alist semantics-units))))
  :config
  (require 'the-org-mode-expansions)
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
                          (?e . er/mark-email))))

(use-package selected
  :ensure t
  :diminish selected-minor-mode selected-global-mode
  :commands selected-minor-mode selected-global-mode
  :init
  (add-hook 'after-init-hook 'selected-global-mode)
  :config
  (defvar selected-org-mode-map (make-sparse-keymap)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode t)
  :config
  (setq projectile-cache-file (expand-file-name (concat user-emacs-directory ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
        projectile-sort-order 'modification-time
        projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t))

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-on
  :init
  (add-hook 'projectile-mode-hook 'counsel-projectile-on)
  (fset 'projectile-find-file        'counsel-projectile-find-file)
  (fset 'projectile-find-dir         'counsel-projectile-find-dir)
  (fset 'projectile-switch-to-buffer 'counsel-projectile-switch-to-buffer)
  (fset 'projectile-ag               'counsel-projectile-ag)
  (fset 'projectile-switch-project   'counsel-projectile-switch-project))

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp)

(use-package anzu
  :ensure t
  :commands global-anzu-mode
  :bind
  (("M-%"   . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)
   ("M-#"   . anzu-replace-at-cursor-thing)
   ("C-M-#" . anzu-query-replace-at-cursor-thing))
  :init
  (add-hook 'spaceline-pre-hook 'global-anzu-mode)
  :config
  (setq anzu-mode-lighter ""
        anzu-replace-to-string-separator " => "))

(use-package eww
  :commands eww eww-mode
  :config
  (setq url-configuration-directory (concat user-emacs-directory ".cache/url")))

(use-package move-dup
  :ensure t
  :commands md/move-lines-up md/move-lines-down md/duplicate-up md/duplicate-down)

(use-package swap-regions
  :ensure t
  :commands swap-regions-mode swap-regions
  :init
  (add-hook 'spaceline-pre-hook 'swap-regions-mode))

(use-package tabbar
  :ensure t
  :commands tabbar-mode
  :init
  (add-hook 'spaceline-pre-hook 'tabbar-mode))

(use-package tabbar-ruler
  :ensure t
  :after tabbar
  :bind
  (("C-x t" . tabbar-ruler-move))
  :config
  (setq tabbar-use-images nil
        tabbar-ruler-global-tabbar t
        tabbar-ruler-global-ruler t))

(use-package package-utils
  :ensure t
  :commands package-utils-install-async package-utils-upgrade-all)

(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  (add-hook 'after-init-hook '(lambda () (when (memq window-system '(mac ns x))
                                      (exec-path-from-shell-initialize))))
  :config
  (setq exec-path-from-shell-check-startup-files nil))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)))
  (dashboard-setup-startup-hook))

(use-package boon
  :ensure t
  :disabled t
  :commands boon-mode
  :init
  (add-hook 'after-init-hook 'boon-mode)
  :config
  (require 'boon-qwerty))

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

(use-package focus
  :ensure t
  :commands focus-mode)

(use-package emr
  :ensure t
  :commands emr-initialize emr-show-refactor-menu
  :init
  (add-hook 'prog-mode-hook 'emr-initialize))

(use-package dumb-jump
  :ensure t
  :bind
  (:map dumb-jump-mode-map
        ("M-g o" . dumb-jump-go-other-window)
        ("M-g j" . dumb-jump-go)
        ("M-g b" . dumb-jump-back)
        ("M-g q" . dumb-jump-quick-look)
        ("C-M-g" . nil)
        ("C-M-p" . nil)
        ("C-M-q" . nil))
  :init
  (add-hook 'prog-mode-hook 'dumb-jump-mode)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package which-func
  :commands which-function-mode
  :init
  (add-hook 'prog-mode-hook 'which-function-mode))

(use-package highlight
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-hi-lock-mode))

(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(eval-after-load "dired" '(use-package dired+ :ensure t :defer 0
                            :init
                            (use-package tramp)
                            :config
                            (setq diredp-image-preview-in-tooltip t)))

(eval-after-load "replace" '(use-package replace+ :ensure t :defer 0))

(eval-after-load "mouse" '(use-package mouse+ :ensure t :defer 0))

(eval-after-load "menu-bar" '(use-package menu-bar+ :ensure t :defer 0))

(eval-after-load "info" '(use-package info+ :ensure t :defer 0))

(eval-after-load "isearch" '(use-package isearch+ :ensure t :defer 0))
(eval-after-load "isearch" '(use-package isearch-prop :ensure t :defer 0))

(use-package bookmark
  :init
  (eval-after-load "bookmark" '(use-package bookmark+ :ensure t :defer 0
                                 :init
                                 (defvaralias 'bmkp-replace-eww-keys-flag 'bmkp-replace-EWW-keys-flag)))
  :config
  (setq bmkp-last-as-first-bookmark-file (concat user-emacs-directory ".cache/bookmarks")))

(use-package imenu
  :init
  (add-hook 'prog-mode-hook 'imenu-add-menubar-index)
  (eval-after-load "imenu" '(use-package imenu+ :ensure t))
  :config
  (setq imenu-auto-rescan t))

(provide 'mds-structure)
;;; mds-structure.el ends here
