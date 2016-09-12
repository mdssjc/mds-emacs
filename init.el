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
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Pacotes
;; Solarized Colorscheme for Emacs
(setq color-themes '())
(use-package color-theme-solarized
  :ensure t
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))

;; Automático
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
