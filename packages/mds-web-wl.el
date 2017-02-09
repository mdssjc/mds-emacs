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
  ("\\.scss$" . web-mode)
  :bind
  (:map web-mode-map
        ("<f6> p" . emmet-preview-mode))
  :init
  (add-hook 'web-mode-hook
            '(lambda () (progn
                     (emmet-mode)
                     (emmet-preview-mode)
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
                     (setq-local counsel-dash-docsets '("HTML" "CSS")))))
  :config
  (require 'html-mode-expansions)
  (require 'css-mode-expansions)
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
        company-minimum-prefix-length 0)
  (er/add-html-mode-expansions)
  (er/add-css-mode-expansions))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :bind
  (:map emmet-mode-keymap
        ("<C-return>" . nil)))

(use-package company-web
  :ensure t
  :commands company-web-html)

(provide 'mds-web-wl)
;;; mds-web-wl.el ends here
