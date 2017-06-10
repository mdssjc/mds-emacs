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
  :commands lsp-mode)

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (require 'lsp-java)
  (require 'lsp-mode)
  (lsp-mode t))

(provide 'mds-experimental)
;;; mds-experimental.el ends here
