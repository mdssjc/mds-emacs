;;; mds-experimental.el --- Em testes. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2018  Marcelo dos Santos
;;
;; Author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Em testes

;;; Code:
(use-package lsp-mode
  :ensure t
  :commands lsp-mode)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp)
  ;:config)
  ;(setq)) ;company-lsp-cache-candidates t
        ;company-lsp-async nil))
        ;company-lsp-enable-snippet t))

(use-package esup
  :ensure t
  :commands esup)

(use-package sicp
  :ensure t
  :defer t)

(provide 'mds-experimental)
;;; mds-experimental.el ends here
