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

(eval-when-compile
  (require 'use-package))
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

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
  :defer t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))
(use-package esup
  :ensure t
  :defer t
  :commands esup)

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
;; F9 - Livre
;; ---

(defun display-extended-command-shorter (command)
  "Display information on a shorter way to M-x a command."
  (interactive (list (read-extended-command)))
  (message "The command `%s' can be invoked with `M-x %s'"
           command
           (execute-extended-command--shorter command command)))

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
(require 'mds-lisp-pl)
(require 'mds-java-pl)
;; (require 'mds-java-pl)
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
 '(package-selected-packages
   (quote
    (markdown-mode meghanada jtags java-snippets rainbow-delimiters geiser parinfer litable org-bullets lispy racket-mode flycheck-pos-tip flycheck-package flycheck auto-yasnippet yasnippet company-emoji company-statistics writeroom-mode which-key use-package undo-tree spacemacs-theme spaceline neotree mode-icons magit hydra golden-ratio general exec-path-from-shell esup emojify counsel-projectile company-quickhelp company-dict centered-cursor-mode boon beacon all-the-icons ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
