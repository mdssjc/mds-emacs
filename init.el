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

;; Configurações Globais
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      column-number-mode t
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
      )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)
                               (untabify (point-min) (point-max))))

;; Atalhos
(load-file "~/.emacs.d/core/funcs.el")
(use-package general
  :ensure t
  :config
  (general-define-key
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   "<C-return>" 'mds/insert-lines-between))

;; Pacotes (Packages)
;; Estético (Aesthetic)
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))
(use-package powerline
  :ensure t
 ; :defer t
  :config
  (powerline-default-theme))
(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))
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
        company-idle-delay 0.1
        company-show-numbers t
        company-backends '((
                            company-abbrev
                            company-bbdb
                            company-capf
                            company-dabbrev-code
                            company-dabbrev
                            company-elisp
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

;; Linguagem de Marcação (Markup Language)
;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Automático (Automatic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powerline company which-key spacemacs-theme general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
