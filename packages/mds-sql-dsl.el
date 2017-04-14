;;; mds-sql-dsl.el --- Linguagem de Domínio Específico para Linguagem de Consulta Estruturada (Domain Specific Language for Structured Query Language)
;;
;; Copyright (C) 2017-2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Configurações para a linguagem SQL.

;;; Code:
(use-package sql
  :mode
  (("\\.sql\\'"  . sql-mode)
   ("\\.zsql\\'" . sql-mode))
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package sqlup-mode
  :ensure t
  :commands sqlup-mode)

(use-package sql-indent
  :ensure t
  :config
  (eval-after-load "sql" '(load-library "sql-indent")))

(provide 'mds-sql-dsl)
;;; mds-sql-dsl.el ends here