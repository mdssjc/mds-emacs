;;; init.el --- Arquivo init (Init file)
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
;; Arquivo de inicialização do editor.

;;; Code:
(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Configurações Globais (Global Settings)
(setq column-number-mode t
      visible-bell t
      ;; Tabs / Indentation
      standard-indent 2
      c-basic-offset 2
      tab-width 2
      indent-tabs-mode nil
      tab-stop-list '(2 4 6)
      tab-always-indent 'complete
      ;; Newline
      require-final-newline t
      next-line-extends-end-of-buffer nil
      next-line-add-newlines nil
      ;; Proxy
      ;;url-proxy-services '(("https" . "127.0.0.1:1234")
      ;;                     ("http"  . "127.0.0.1:1234"))
      ;; Smooth Scrolling
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position 't)

(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/packages/")
(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

(mouse-avoidance-mode 'animate)
(fset 'yes-or-no-p 'y-or-n-p)
(save-place-mode 1)

;; https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; Segredos (Secrets)
(load "~/.emacs.d/secrets/secrets")

;; Atalhos
(use-package general
  :ensure t
  :config
  (load "mds-core-funcs")
  (general-define-key
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   "<C-return>" 'mds/insert-lines-between
   "M-/"        'hippie-expand))
;; F5 - Toggles Global
(use-package golden-ratio :ensure t :diminish " φ"
  :bind (("<f5> g" . golden-ratio-mode)))
(use-package centered-cursor-mode :ensure t :diminish " ⊝"
  :bind (("<f5> -" . centered-cursor-mode)))
(use-package boon :ensure t
  :bind (("<f5> b" . boon-mode))
  :config
  (require 'boon-colemak)
  (add-to-list 'boon-special-mode-list 'emacs-lisp-mode))
(use-package writeroom-mode :ensure t :diminish
  :bind (("<f5> w" . writeroom-mode)))
;; F6 - Toggles do Modo Maior
;; - configurados em cada pacote
;; F7 - Aplicações Interna
;; F8 - Aplicações Externa
;; ---

;; Estético (Aesthetic)
(load "mds-aesthetic")
;; Estrutura (Structure)
(load "mds-structure")
;; Sintaxe (Syntax)
(load "mds-syntax")
;; Semântico (Semantic)
(load "mds-semantic")
;; Pragmático (Pragmatic)
(load "mds-pragmatic")
;; Linguagem de Programação (Programming Language)
(load "mds-lisp-pl")
(load "mds-java-pl")
;; (require 'mds-c-pl)
;; (require 'mds-web-pl)
;; Linguagem de Marcação (Markup Language)
(load "mds-markdown-ml")
;; ---

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "423caec5827def4b51faf4e7faf194630b8df539d6f927a822d42d5bf86aa5a7" default)))
 '(package-selected-packages
   (quote
    (smart-mode-line writeroom-mode neotree geiser flycheck-pos-tip markdown-mode which-key use-package spacemacs-theme spaceline rainbow-delimiters racket-mode mode-icons meghanada magit lispy golden-ratio general flycheck-package counsel-projectile company-statistics company-quickhelp company-dict centered-cursor-mode boon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
