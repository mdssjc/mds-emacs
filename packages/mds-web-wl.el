;;; mds-web-wl.el --- Linguagem Web (Web Language) -*- lexical-binding: t -*-
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
;; Configurações para a linguagem Web.

;;; Code:
(use-package web-mode
  :ensure t
  :mode
  (("\\.html$" . web-mode)
   ("\\.htm$"  . web-mode)
   ("\\.jsp$"  . web-mode))
  :init
  (add-hook 'web-mode-hook
            '(lambda ()
               (emmet-mode)
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-web-html
                                               company-yasnippet
                                               :with
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))
               (flycheck-mode)
               (setq-local counsel-dash-docsets '("HTML" "CSS" "Sass"))))
  :config
  (require 'html-mode-expansions)
  (er/add-html-mode-expansions)
  (setq web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-expanding t
        emmet-indentation 2
        company-minimum-prefix-length 0))

(use-package css-mode
  :mode
  (("\\.css$"  . css-mode)
   ("\\.scss$" . css-mode))
  :init
  (add-hook 'css-mode-hook
            '(lambda ()
               (emmet-mode)
               (setq-local imenu-create-index-function
                           '(lambda () (save-excursion)
                              (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))
               (setq-local company-transformers '(company-sort-prefer-same-case-prefix))
               (setq-local company-backends '((company-css
                                               company-yasnippet)))
               (css-eldoc-enable)
               (setq-local counsel-dash-docsets '("CSS" "Sass"))))
  :config
  (require 'css-mode-expansions)
  (er/add-css-mode-expansions)
  (setq css-indent-offset 2
        emmet-indentation 2
        emmet-preview-default t))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode)

(use-package company-web
  :ensure t
  :commands company-web-html)

(use-package css-eldoc
  :ensure t
  :after css-mode)

(use-package restclient
  :ensure t
  :commands restclient-mode
  :init
  (add-hook 'restclient-mode-hook
            '(lambda ()
               (setq-local company-backends '((company-restclient))))))

(use-package company-restclient
  :ensure t
  :commands company-restclient)

(provide 'mds-web-wl)
;;; mds-web-wl.el ends here
