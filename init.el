;;; init.el --- Arquivo init (Init file) -*- lexical-binding: t -*-
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
;; Arquivo de inicialização do ambiente Emacs.

;;; Code:
(setq load-prefer-newer t
      debug-on-error nil)

;; ----------------------
;; GC: Garbage Collection
;; ----------------------
(let ((init-gc-cons-threshold   (* 128 1024 1024))
      (normal-gc-cons-threshold (*  20 1024 1024)))
  (setq gc-cons-threshold  init-gc-cons-threshold
        gc-cons-percentage 0.1
        garbage-collection-messages nil)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
;; ---

;; ------------------
;; Pacotes (Packages)
;; ------------------
(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "core/use-package/"))
  (require 'use-package))

(use-package package
  :init
  (setq package-enable-at-startup nil
        package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/"))
        use-package-verbose nil
        use-package-compute-statistics t
        use-package-enable-imenu-support t)
  (package-initialize nil))
(use-package use-package-ensure-system-package)
(use-package system-packages :ensure t)
(use-package diminish :ensure t)
;; ---

;; (let ((file-name-handler-alist))
;; ---------------------------------------
;; Configurações Globais (Global Settings)
;; ---------------------------------------
(setq-default tab-always-indent 'complete
              ;; Indentation
              indent-tabs-mode nil
              standard-indent 2
              tab-width       2
              c-basic-offset  2
              tab-stop-list '(2 4 6 8 10 12 14))

(setq initial-major-mode 'fundamental-mode
      inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      visible-bell nil
      ;; UI
      bidi-display-reordering nil
      cursor-in-non-selected-windows nil
      use-dialog-box nil
      ;; Garbage Collect
      jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      ;; Newline
      require-final-newline t
      indicate-empty-lines t
      next-line-extends-end-of-buffer nil
      next-line-add-newlines nil
      ;; Proxy
      ;;url-proxy-services '(("https" . "127.0.0.1:1234")
      ;;                     ("http"  . "127.0.0.1:1234"))
      ;; Scrolling
      hscroll-margin 1
      hscroll-step 1
      scroll-conservatively 1001
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Bookmark
      bookmark-default-file (concat user-emacs-directory ".cache/bookmarks")
      bookmark-save-flag t
      bmkp-last-as-first-bookmark-file nil
      ;; Paths
      tramp-persistency-file-name (concat user-emacs-directory ".cache/tramp")
      semanticdb-default-save-directory (concat user-emacs-directory ".cache/semanticdb")
      url-configuration-directory (concat user-emacs-directory ".cache/url"))

;; Sistema de Codificação (Coding System)
(set-charset-priority        'unicode)
(set-default-coding-systems  'utf-8)
(prefer-coding-system        'utf-8)
(set-language-environment    'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-input-method nil)
;; ---

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(mouse-avoidance-mode 'animate)

;; Alias
(fset 'yes-or-no-p 'y-or-n-p)
(if (not (version< emacs-version "26"))
    (progn
      (fset 'display-buffer-in-major-side-window 'window--make-major-side-window)
      (fset 'defmethod 'cl-defmethod)
      (fset 'defgeneric 'cl-defgeneric)
      (fset 'if-let 'if-let*)
      (fset 'when-let 'when-let*)))
(fset 'cl--copy-slot-descriptor-1 'copy-sequence)

(defun display-extended-command-shorter (command)
  "Display information on a shorter way to M-x a command."
  (interactive (list (read-extended-command)))
  (message "The command `%s' can be invoked with `M-x %s'"
           command
           (execute-extended-command--shorter command command)))
;; ---

;; OS
(cond ((eq system-type 'gnu/linux)
       (setq x-gtk-use-system-tooltips nil))
      ((eq window-system 'w32)
       (progn
         (setq w32-pass-lwindow-to-system nil
               w32-pass-rwindow-to-system nil
               w32-pass-apps-to-system    nil
               w32-lwindow-modifier       'nil
               w32-rwindow-modifier       'nil
               w32-apps-modifier          'nil)
         (define-key key-translation-map (kbd "<lwindow>") 'event-apply-super-modifier)
         (define-key key-translation-map (kbd "<rwindow>") 'event-apply-super-modifier)
         (define-key key-translation-map (kbd "<apps>")    'event-apply-hyper-modifier))))
;; ---

;; Segredos (Secrets)
(if (file-exists-p (concat user-emacs-directory "secrets/secrets.el"))
    (load (concat user-emacs-directory "secrets/secrets"))
  (message "Use mds-secrets-template.el as the basis for the secrets.el file."))
;; ---

;; Pacotes Essenciais (Essential Packages)
(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path (concat user-emacs-directory "packages"))
(require 'mds-core-funcs)
;; ---

;; Pacotes (Packages)
;; Estético (Aesthetic)
(require 'mds-aesthetic)
;; Estrutura (Structure)
(require 'mds-structure)
;; Atalhos (Shortcuts)
(require 'mds-shortcuts)
(require 'mds-hydra)
;; Sintaxe (Syntax)
(require 'mds-syntax)
;; Semântico (Semantic)
(require 'mds-semantic)
;; Pragmático (Pragmatic)
(require 'mds-pragmatic)
;; Terminal (Terminal)
(require 'mds-terminal)
;; Linguagem de Programação Geral (General Programming Language)
(require 'mds-java-pl)
(require 'mds-javascript-pl)
(require 'mds-haskell-pl)
(require 'mds-lisp-pl)
(require 'mds-c-pl)
;; (require 'mds-red-pl)
;; Linguagem Web (Web Language)
(require 'mds-web-wl)
;; Linguagem de Marcação (Markup Language)
(require 'mds-markdown-ml)
;; Linguagem de Domínio Específico (Domain Specific Language)
(require 'mds-r-dsl)
(require 'mds-sql-dsl)
(require 'mds-plantuml-dsl)
;; Serviços
(require 'mds-news)
;; Experimental
(require 'mds-experimental)

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ---

;;; init.el ends here
