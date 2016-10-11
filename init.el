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
      initial-major-mode 'fundamental-mode
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
      ;; Backups and Autosave
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory
                                                                 ".cache/backups"))))
      auto-save-file-name-transforms `((".*" ,(expand-file-name (concat user-emacs-directory
                                                                        ".cache/autosave")))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/packages/")
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
  :config
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay 0.15)
  (key-chord-mode 1))

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
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

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

;; Segredos (Secrets)
(load "~/.emacs.d/secrets/secrets")
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
    (engine-mode engine counsel-dash zeal-at-point javadoc-lookup visual-regexp visual-reexp counsel-projectile markdown-mode emmet-mode web-mode meghanada jtags auto-yasnippet java-snippets geiser litable racket-mode rainbow-delimiters lispy parinfer paredit org-bullets langtool flycheck-pos-tip flycheck-package flycheck yasnippet company-math company-emoji company-statistics company-quickhelp company-dict company ripgrep projectile selected embrace hydra counsel swiper ace-window avy magit git-gutter-fringe neotree undo-tree restart-emacs emojify beacon all-the-icons mode-icons info+ spaceline spacemacs-theme writeroom-mode boon centered-cursor-mode golden-ratio general esup powerline exec-path-from-shell use-package-chords f s dash which-key use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ---

;;; init.el ends here
