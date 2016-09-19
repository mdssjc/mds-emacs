;;
;; Init.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
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
;; F5 ao F8: definir funcionalidades
;; ---

;; Estético (Aesthetic)
(require 'aesthetic)
;; Estrutura (Structure)
(require 'structure)
;; ---

;; Sintaxe (Sintaxe)
;; Autocompletar (Autocomplete)
;; Company
(use-package company
  :ensure t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq tab-always-indent 'complete
        company-tooltip-limit 10
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-auto-complete nil
        company-idle-delay 0.05
        company-show-numbers t
        company-echo-delay 0
        company-dabbrev-other-buffers 'all
        company-backends '((company-files
                            company-abbrev
                            company-dabbrev-code
                            company-dabbrev
                            company-keywords
                            company-capf
                            company-semantic
                            company-bbdb
                            company-etags
                            company-gtags
                            company-tempo
                            company-dict
                            company-yasnippet
                            company-ispell))))
(use-package company-dict               ; company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil
        company-dict-dir (concat user-emacs-directory "dict/")))
(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode 1))
(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))
;; Correção (Correction)
;; Ispell
;; - instale o Hunspell
;; - entre com o comando Hunspell -D
;; - escolha a URI do dicionário com a extensão .dic
;; - entre com o pipe de comandos cat URI/XXX.dic | cut -d"/" -f1 > ~/.emacs.d/dicts/XXX
;; - converta a codificação do dicionário com o comando iconv -f ENCODE -t UTF-8 -o XXX
;; - configure a variável company-ispell-dictionary com o dicionário
;; - avançado: obtenha o dicionário pelo LanguageTool
(use-package ispell                     ; company-ispell
  :ensure t
  :after company
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pt_BR"
        ispell-really-hunspell t
        ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/pt_BR.dic"))
;; Abreviação (Abbreviation)

;; Template
;; Yasnippet
(use-package yasnippet                  ; company-yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after company
  :config
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-completing-prompt
                               yas-no-prompt
                               yas-x-prompt))
  (yas-global-mode 1)
  (yas-reload-all))
;; ---

;; Linguagem de Programação (Programming Language)
(require 'programming-language-lisp)
;; ---

;; Java
;; (use-package meghanada ; Bug
;;   :ensure t
;;   :init (add-hook 'java-mode-hook (lambda () (meghanada-mode t))))
;; ---

;; Linguagem de Marcação (Markup Language)
;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))
;; ---

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode racket-mode rainbow-delimiters lispy yasnippet company-statistics company-quickhelp company-dict company counsel swiper ivy ace-window avy magit expand-region which-key mode-icons spaceline spacemacs-theme general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
