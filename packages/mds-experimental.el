;;; mds-experimental.el --- Em testes.               -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017  Marcelo dos Santos
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
  :commands lsp-mode
  :init
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck)))

(use-package esup
  :ensure t
  :commands esup)

(provide 'mds-experimental)
;;; mds-experimental.el ends here
