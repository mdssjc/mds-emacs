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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-header-function            #'treemacs--create-header-projectile
        treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package esup
  :ensure t
  :commands esup)

(provide 'mds-experimental)
;;; mds-experimental.el ends here
