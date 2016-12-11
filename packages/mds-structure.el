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
;;  - restart-emacs: funcionalidade de reinicialização;
;;  - undo-tree: visualização da árvore de modificações do buffer;
;;  - keyfreq: sumariza a frequência de utilização dos atalhos;
;;  - neotree: visualização do sistema de arquivos;
;;  - magit: controle de versão pelo Git;
;;  - pacotes Abo-abo: Avy, Hydra, Ace-Window, Ivy, Swiper e Counsel;
;;  - Seleção de partes do buffer com funcionalidades;
;;  - Navegador de projetos;
;;  - ripgrep: ferramenta de busca Ripgrep (rg);
;;  - visual-regexp: busca/substituição com indicação visual;
;;  - Browser interno;
;;  - exec-path-from-shell: variáveis ambiente do shell.

;;; Code:
(use-package saveplace
  :commands save-place-mode
  :init
  (add-hook 'after-init-hook 'save-place-mode)
  :config
  (setq save-place-file (expand-file-name (concat user-emacs-directory ".cache/places"))))

(use-package recentf
  :commands recentf-mode
  :init
  (add-hook 'after-init-hook 'recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name (concat user-emacs-directory ".cache/recentf"))
        recentf-max-saved-items 1000
        recentf-max-menu-items 15))

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
  :config
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
  :after magit
  :config
  (setq git-gutter-fr:side 'right-fringe
        git-gutter:update-interval 5)
  (global-git-gutter-mode t))

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
  :ensure t
  :defer t)

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
        projectile-completion-system   'ivy
        magit-completing-read-function 'ivy-completing-read))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package swiper
  :ensure t
  :config
  (setq swiper-include-line-number-in-search t))

(use-package counsel
  :ensure t
  :defer t)

(use-package smex
  :ensure t)

(use-package expand-region
  :ensure t
  :commands er/expand-region)

(use-package embrace
  :ensure t
  :init
  (add-hook 'text-mode-hook '(lambda () (setq embrace-semantic-units-alist
                                         (append embrace-semantic-units-alist semantics-units))))
  (add-hook 'prog-mode-hook '(lambda () (setq embrace-semantic-units-alist
                                         (append embrace-semantic-units-alist semantics-units))))
  :config
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

;; Projeto
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("<f7> p" . projectile-command-map))
  :init
  (projectile-mode t)
  :config
  (setq projectile-cache-file (expand-file-name (concat user-emacs-directory ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
        projectile-sort-order 'modification-time
        projectile-completion-system 'ivy
        projectile-switch-project-action 'neotree-projectile-action))

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile-on
  :init
  (add-hook 'projectile-mode-hook 'counsel-projectile-on))
;; ---

(use-package ripgrep
  :ensure t
  :commands ripgrep-regexp
  :bind
  (("<f8> r" . ripgrep-regexp)))

(use-package visual-regexp
  :ensure t
  :commands vr/replace vr/query-replace)

;; Browser
(use-package eww
  :commands eww eww-mode
  :bind
  (("<f7> b e" . eww))
  :config
  (setq url-configuration-directory (concat user-emacs-directory ".cache/url")))

;; Imenu
(use-package flx-ido
    :ensure t)

(use-package popup-imenu
  :ensure t
  :bind
  (("C-'" . popup-imenu)
   :map popup-isearch-keymap
   ("C-'" . popup-isearch-cancel))
  :config
  (setq popup-imenu-style 'indent))

;; Move-dup
(use-package move-dup
  :ensure t
  :bind
  (("M-<up>"     . md/move-lines-up)
   ("M-<down>"   . md/move-lines-down)
   ("S-M-<up>"   . md/duplicate-up)
   ("S-M-<down>" . md/duplicate-down)))
;; ---

;; Swap
(use-package swap-regions
  :ensure t
  :commands swap-regions-mode
  :bind
  (("S-M-t" . swap-regions))
  :init
  (add-hook 'after-init-hook 'swap-regions-mode))
;; ---

;; Tabbar
(use-package tabbar
  :ensure t
  :commands tabbar-mode
  :init
  (add-hook 'after-init-hook 'tabbar-mode))
;; ---

;; Package Utils
(use-package package-utils
  :ensure t
  :bind
  (("<f7> u a" . package-utils-install-async)
   ("<f7> u u" . package-utils-upgrade-all)))
;; ---

;; Purcell (https://github.com/purcell)
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)))
  (dashboard-setup-startup-hook))
;; ---

(use-package boon
  :ensure t
  :disabled t
  :commands boon-mode
  :bind
  (:map boon-command-map
        ("S" . embrace-commander))
  :init
  (add-hook 'after-init-hook 'boon-mode)
  ;; (define-key boon-command-map "S" 'embrace-commander)
  :config
  (require 'boon-qwerty))

(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

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

;; dired
(use-package dired-icon
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'dired-icon-mode))
;; ---

(use-package replace+
  :ensure t
  :defer t)

(use-package mouse+
  :ensure t
  :defer t)

(use-package menu-bar+
  :ensure t
  :defer t)

(use-package info+
  :ensure t
  :defer t)

(use-package isearch+
  :ensure t
  :defer t)

(provide 'mds-structure)
;;; mds-structure.el ends here
