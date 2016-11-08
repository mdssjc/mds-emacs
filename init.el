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
;; Arquivo de inicialização do ambiente.

;;; Code:
;; Bootstrap `use-package'
(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
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
(setq-default tab-always-indent 'complete
              ;; Indentation
              indent-tabs-mode nil
              standard-indent 2
              tab-width 2
              c-basic-offset 2
              tab-stop-list '(2 4 6 8 10 12))
(setq initial-major-mode 'fundamental-mode
      column-number-mode t
      visible-bell t
      ;; Garbage Collect
      gc-cons-threshold (* 100 1024 1024) ; 100 MB
      jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
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
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory
                                                                 ".cache/backups"))))
      make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      ;; Autosave
      auto-save-file-name-transforms `((".*" ,(expand-file-name (concat user-emacs-directory
                                                                        ".cache/auto-save"))))
      auto-save-list-file-prefix (concat user-emacs-directory
                                         ".cache/auto-save-list/.saves-")
      auto-save-default t
      auto-save-timeout 60
      auto-save-interval 50)

;; Sistema de Codificação (Coding System)
(set-charset-priority        'unicode)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; --

(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path (concat user-emacs-directory "packages"))
;; ---

;; Pacotes Essenciais (Essential Packages)
(require 'cl)

(use-package dash
  :ensure t
  :config
  (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

(require 'mds-core-funcs)

(use-package use-package-chords
  :ensure t
  :init
  (add-hook 'after-init-hook '(lambda () (key-chord-mode t)))
  :config
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay  0.15))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package esup
  :ensure t
  :commands esup)
;; ---

;; Hooks
(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook  'delete-trailing-whitespace)

(mouse-avoidance-mode 'animate)
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'display-buffer-in-major-side-window 'window--make-major-side-window)

(defun display-extended-command-shorter (command)
  "Display information on a shorter way to M-x a command."
  (interactive (list (read-extended-command)))
  (message "The command `%s' can be invoked with `M-x %s'"
           command
           (execute-extended-command--shorter command command)))
;; ---

;; Server
(require 'server nil t)
(use-package server
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))
;; --

;; OS
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips t))
;; --

;; Segredos (Secrets)
(load (concat user-emacs-directory "secrets/secrets"))
;; ---

;; Pacotes (Packages)
;; Estético (Aesthetic)
(require 'mds-aesthetic)
;; Estrutura (Structure)
(require 'mds-structure)
;; Atalhos (Shortcuts)
(require 'mds-shortcuts)
;; Sintaxe (Syntax)
(require 'mds-syntax)
;; Semântico (Semantic)
(require 'mds-semantic)
;; Pragmático (Pragmatic)
(require 'mds-pragmatic)
;; Linguagem de Programação (Programming Language)
(require 'mds-lisp-pl)
(require 'mds-haskell-pl)
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
    (markdown-mode emmet-mode web-mode jdee zeal-at-point writeroom-mode which-key volatile-highlights visual-regexp use-package-chords undo-tree swap-regions spacemacs-theme spaceline shm selected ripgrep restart-emacs rainbow-delimiters racket-mode popup-imenu parinfer org-bullets neotree move-dup mode-icons meghanada magit litable lispy langtool keyfreq java-snippets info+ haskell-snippets golden-ratio git-timemachine git-gutter-fringe general focus flyspell-popup flyspell-correct-popup flyspell-correct-ivy flycheck-pos-tip flycheck-package flycheck-haskell f exec-path-from-shell esup engine-mode emojify embrace dr-racket-like-unicode counsel-projectile counsel-dash company-statistics company-quickhelp company-ghc company-dict cider centered-cursor-mode beacon auto-yasnippet all-the-icons))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ---

;;; init.el ends here
