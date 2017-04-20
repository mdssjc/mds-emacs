;;; init.el --- Arquivo init (Init file)
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
(setq gc-cons-threshold (* 100 1024 1024)
      load-prefer-newer t
      debug-on-error nil
      use-package-verbose nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let ((file-name-handler-alist))
  ;; Configurações Globais (Global Settings)
  (setq-default tab-always-indent 'complete
                ;; Indentation
                indent-tabs-mode nil
                standard-indent 2
                tab-width 2
                c-basic-offset 2
                tab-stop-list '(2 4 6 8 10 12))

  (setq initial-major-mode 'fundamental-mode
        inhibit-startup-screen t
        column-number-mode t
        size-indication-mode t
        visible-bell t
        ;; Garbage Collect
        jit-lock-defer-time nil
        jit-lock-stealth-nice 0.1
        jit-lock-stealth-time 0.2
        jit-lock-stealth-verbose nil
        ;; Newline
        indicate-empty-lines t
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
        ;; Bookmark
        bookmark-default-file (concat user-emacs-directory ".cache/bookmarks")
        bmkp-last-as-first-bookmark-file nil)

  ;; Sistema de Codificação (Coding System)
  (set-charset-priority        'unicode)
  (set-default-coding-systems  'utf-8)
  (prefer-coding-system        'utf-8)
  (set-language-environment    'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; ---

  ;; Hooks
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (mouse-avoidance-mode 'animate)
  (fset 'yes-or-no-p 'y-or-n-p)
  (fset 'display-buffer-in-major-side-window 'window--make-major-side-window)
  (fset 'cl--copy-slot-descriptor-1 'copy-sequence)

  (defun display-extended-command-shorter (command)
    "Display information on a shorter way to M-x a command."
    (interactive (list (read-extended-command)))
    (message "The command `%s' can be invoked with `M-x %s'"
             command
             (execute-extended-command--shorter command command)))
  ;; ---

  ;; OS
  (when (eq system-type 'gnu/linux)
    (setq x-gtk-use-system-tooltips t))
  ;; ---

  ;; Segredos (Secrets)
  (load (concat user-emacs-directory "secrets/secrets"))
  ;; ---

  ;; Pacotes Essenciais (Essential Packages)
  (add-to-list 'load-path (concat user-emacs-directory "core"))
  (add-to-list 'load-path (concat user-emacs-directory "packages"))
  (require 'mds-core-funcs)

  (use-package esup
    :ensure t
    :commands esup)
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
  (require 'mds-news))

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zop-to-char zeal-at-point wttrin writeroom-mode worf which-key wgrep web-mode volatile-highlights visual-regexp use-package undo-tree twittering-mode tomatinho tao-theme tabbar-ruler symon swap-regions srefactor sqlup-mode sql-indent spaceline-all-the-icons smex simple+ shm shift-number rg restart-emacs replace+ rainbow-delimiters racket-mode projectile-speedbar projectile-ripgrep pp+ popup-imenu popup-edit-menu plantuml-mode parinfer package-utils org-table-sticky-header org-pomodoro org-bullets move-dup mouse+ menu-bar+ meghanada markdown-mode magit litable lispy lfe-mode langtool lacarte jdee java-snippets ivy-rich ivy-hydra isearch-prop isearch+ irony-eldoc intero info+ imenu+ icicles hlint-refactor hl-line+ highlight-thing haskell-snippets guess-language google-translate google-this golden-ratio git-timemachine git-gutter-fringe general function-args focus flyspell-popup flyspell-correct-ivy flycheck-pos-tip flycheck-package flycheck-irony flycheck-haskell face-remap+ exec-path-from-shell esup ess eshell-fringe-status erefactor engine-mode emr emojify emmet-mode embrace elfeed electric-spacing dumb-jump dr-racket-like-unicode dired+ dashboard counsel-projectile counsel-dash company-web company-statistics company-quickhelp company-irony-c-headers company-irony company-ghci company-ghc company-dict company-cabal color-identifiers-mode clj-refactor ciel cider-eval-sexp-fu centered-cursor-mode bug-hunter buffer-move bookmark+ auto-yasnippet anzu all-the-icons-dired))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ---

;;; init.el ends here
