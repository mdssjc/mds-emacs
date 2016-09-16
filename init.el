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

(eval-when-compile
  (require 'use-package))

;; Configurações Globais (Global Settings)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      column-number-mode t
      visible-bell t
      ;; Tabs / Indentation
      standard-indent 2
      tab-width 2
      indent-tabs-mode nil
      tab-stop-list '(2  4  6)
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

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'window-setup-hook 'toggle-frame-maximized)
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

;; Segredos (Secrets)
(load "~/.emacs.d/secrets/secrets")

;; Atalhos
;;(load "~/.emacs.d/core/funcs")
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

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

;; Pacotes (Packages)
;; Estético (Aesthetic)
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))
(use-package powerline
  :ensure t
  :config (powerline-default-theme))
(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line
;; (use-package all-the-icons
;;   :ensure t)
;; ---

;; Which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;; Company
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-auto-complete nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.05
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-show-numbers t
        tab-always-indent 'complete
        company-backends '((
                            company-abbrev
                            company-bbdb
                            company-capf
                            company-dabbrev-code
                            company-dabbrev
                            ;company-elisp
                            company-etags
                            company-files
                            company-gtags
                            ;company-ispell
                            company-keywords
                            company-oddmuse
                            ;company-semantic
                            ;company-template
                            company-tempo
                            ;company-yasnippet
                            ;company-dict
                            ))))

;; Abo-abo
(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char-timer)
  :config (setq avy-timeout-seconds 0.3))
(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :config (setq aw-dispatch-always t))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-height 5
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))
(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)))
;; ---

;; Linguagem de Programação (Programming Language)
;; Emacs Lisp (ELisp)
(use-package emacs-lisp-mode
  :config
  (progn
    (use-package eldoc
      :config (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))
    (use-package company
      :config (add-to-list 'company-backends 'company-elisp))
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))
(use-package lispy
  :ensure t
  :defer 5
  :diminish lispy-mode
  ;; :bind
  ;; (("s-<right>"   . lispy-)
  ;;  ("S-s-<right>" . sp-forward-barf-sexp)
  ;;  ("s-<left>"    . sp-backward-slurp-sexp)
  ;;  ("S-s-<left>"  . sp-backward-barf-sexp)
  ;;  ;; ("s-<up>"      . sp-wrap-with-pair "(")
  ;; ("s-<down>"    . sp-unwrap-sexp))
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  lisp-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1)))))
(use-package rainbow-delimiters
  :ensure t
  :defer 5
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode t))))
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

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel markdown-mode rainbow-delimiters lispy avy ivy company which-key mode-icons powerline spacemacs-theme general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
