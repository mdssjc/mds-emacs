;;; mds-javascript-pl.el --- Linguagem de Programação JavaScript (JavaScript Programming Language) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Configurações para a linguagem JavaScript.

;;; Code:
(use-package js2-mode
  :ensure t
  :mode
  (("\\.js\\'" . js2-mode))
  :interpreter
  ("node" . j2-mode)
  :init
  (add-hook 'js2-mode-hook
            '(lambda ()
               (js2-imenu-extras-mode)
               (js2-refactor-mode)
               (add-hook 'xref-backend-functions 'xref-js2-xref-backend nil t)
               (tern-mode t)
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-tern
                                               ;;company-capf
                                               ;;company-abbrev
                                               ;;company-dabbrev-code
                                               ;;company-dabbrev
                                               company-files))))))

(use-package js2-refactor
  :ensure t
  :after js2-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :ensure t
  :after js2-mode)

(use-package tern
  :ensure t
  :commands tern-mode)

(use-package company-tern
  :ensure t
  :commands company-tern)

(provide 'mds-javascript-pl)
;;; mds-javascript-pl.el ends here
