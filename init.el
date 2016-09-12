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

(require 'use-package)

;; Configurações Globais
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      standard-indent 2
      tab-width 2
      indent-tabs-mode nil
      tab-stop-list '(2  4  6)
      tab-always-indent 'complete
      column-number-mode t)

;; Pacotes
;; Solarized Colorscheme for Emacs
(use-package solarized-theme
  :ensure t
  :init
  (progn
    (setq solarized-use-less-bold t
          solarized-use-more-italic t
          solarized-emphasize-indicators nil
          solarized-distinct-fringe-background t
          solarized-high-contrast-mode-line t)
    (load-theme 'solarized-dark t))
  :config
  (setq color-theme-is-global t))

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
        company-idle-delay 0.5
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

;; Automático
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (solarized-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
