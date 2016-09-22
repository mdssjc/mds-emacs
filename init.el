;;
;; Init.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Configurações Globais (Global Settings)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      column-number-mode t
      visible-bell t
      ;; Tabs / Indentation
      standard-indent 2
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
      redisplay-dont-pause t
      scroll-margin 1
      scroll-conservatively 10000
      scroll-step 1
      auto-window-vscroll nil)

(add-to-list 'load-path "~/.emacs.d/packages/")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

(mouse-avoidance-mode 'animate)
(fset 'yes-or-no-p 'y-or-n-p)

;; https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

;; Segredos (Secrets)
(load "~/.emacs.d/secrets/secrets")

;; Atalhos
(use-package general
  :ensure t
  :config
  (load "~/.emacs.d/core/funcs")
  (general-define-key
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   "<C-return>" 'mds/insert-lines-between
   "M-/"        'hippie-expand))
;; F6 ao F8: definir funcionalidades
;; F5- Toggles
(use-package golden-ratio :ensure t :diminish " φ"
  :bind (("<f5> g" . golden-ratio-mode)))
(use-package centered-cursor-mode :ensure t :diminish " ⊝"
  :bind (("<f5> -" . centered-cursor-mode)))
(use-package boon :ensure t
  :bind (("<f5> b" . boon-mode))
  :config
  (require 'boon-colemak)
  (add-to-list 'boon-special-mode-list 'emacs-lisp-mode))
;; ---

;; Estético (Aesthetic)
(load "aesthetic")
;; Estrutura (Structure)
(load "structure")
;; Sintaxe (Syntax)
(load "syntax")
;; Semântico (Semantic)
;; (load "semantic")
;; Pragmático (Pragmatic)
;; (load "pragmatic")
;; Linguagem de Programação (Programming Language)
(load "programming-language-lisp")
(load "programming-language-java")
;; (require 'programming-language-c)
;; (require 'programming-language-web)
;; Linguagem de Marcação (Markup Language)
(load "markup-language-markdown")
;; ---

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode meghanada rainbow-delimiters lispy racket-mode yasnippet company-statistics which-key use-package spacemacs-theme spaceline mode-icons magit golden-ratio general counsel-projectile company-quickhelp company-dict centered-cursor-mode boon ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
