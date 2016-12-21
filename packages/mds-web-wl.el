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
  ("\\.html$" . web-mode)
  ("\\.htm$"  . web-mode)
  ("\\.css$"  . web-mode)
  :bind
  (:map web-mode-map
        ("<F6> p" . emmet-preview-mode))
  :init
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook '(lambda () (setq-local counsel-dash-docsets '("HTML" "CSS"))))
  (add-hook 'web-mode-hook '(lambda ()
                              (set (make-local-variable 'company-transformers)
                                   '(company-sort-by-backend-importance
                                     company-sort-prefer-same-case-prefix
                                     company-sort-by-statistics))
                              (set (make-local-variable 'company-backends)
                                   '((company-web-html
                                      company-css
                                      company-yasnippet
                                      :with
                                      company-abbrev
                                      company-dabbrev-code
                                      company-dabbrev
                                      company-files)))))
  :config
  (setq web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-css-colorization t))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :bind
  (:map emmet-mode-keymap
   ("<C-return>" . nil))
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2))

(use-package company-web
  :ensure t)

(provide 'mds-web-wl)
;;; mds-web-wl.el ends here
