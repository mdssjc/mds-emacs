;;; mds-web-wl.el --- Linguagem Web (Web Language)
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
;; Configurações para a linguagem Web.

;;; Code:
(use-package web-mode
  :ensure t
  :mode
  ("\\.\\(html\\|htm\\)$" . web-mode)
  :config
  (setq web-mode-style-padding ers-tab-size
        web-mode-script-padding ers-tab-size))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'html-mode-hook 'emmet-mode))

(provide 'mds-web-wl)
;;; mds-web-wl.el ends here
