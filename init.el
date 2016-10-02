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
;; Bootstrap `use-package'
(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; ---

;; Configurações Globais (Global Settings)
(setq gc-cons-threshold 50000000
      column-number-mode t
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
      scroll-preserve-screen-position 't
      ;; Backups
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backups")))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/packages/")
;; ---

;; Pacotes Essenciais
(require 'cl)

(use-package dash
  :ensure t
  :config
  (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.05)
  (key-chord-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package powerline
  :ensure t)

(use-package esup
  :ensure t
  :commands esup)
;; ---

(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

(mouse-avoidance-mode 'animate)
(fset 'yes-or-no-p 'y-or-n-p)

;; Segredos (Secrets)
(load "~/.emacs.d/secrets/secrets")

;; Atalhos
;; F5 - Toggles Global
;; F6 - Toggles do Modo Maior (em cada pacote)
;; F7 - Aplicações Interna (em estrutura)
;; F8 - Aplicações Externa (em estrutura)
;; F9 - Planejamento
(use-package general
  :ensure t
  :config
  (require 'mds-core-funcs)
  (general-define-key
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   "<C-return>" 'mds/insert-lines-between
   "M-/"        'hippie-expand))
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
;; ---

(defun display-extended-command-shorter (command)
  "Display information on a shorter way to M-x a command."
  (interactive (list (read-extended-command)))
  (message "The command `%s' can be invoked with `M-x %s'"
           command
           (execute-extended-command--shorter command command)))

;; Estético (Aesthetic)
(require 'mds-aesthetic)
;; Estrutura (Structure)
(require 'mds-structure)
;; Sintaxe (Syntax)
(require 'mds-syntax)
;; Semântico (Semantic)
(require 'mds-semantic)
;; Pragmático (Pragmatic)
(require 'mds-pragmatic)
;; Linguagem de Programação (Programming Language)
(require 'mds-lisp-pl)
(require 'mds-java-pl)
(require 'mds-c-pl)
;; Linguagem Web (Web Language)
(require 'mds-web-wl)
;; Linguagem de Marcação (Markup Language)
(require 'mds-markdown-ml)
;; ---

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode emmet-mode web-mode meghanada jtags auto-yasnippet java-snippets rainbow-delimiters geiser litable racket-mode lispy parinfer paredit org-bullets langtool flycheck-pos-tip flycheck-package flycheck yasnippet company-math company-emoji company-flx company-statistics company-quickhelp company-dict company ripgrep projectile hydra counsel swiper ace-window avy magit neotree embrace which-key undo-tree restart-emacs emojify beacon all-the-icons mode-icons info+ spaceline spacemacs-theme writeroom-mode boon centered-cursor-mode golden-ratio general esup powerline exec-path-from-shell use-package-chords use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
