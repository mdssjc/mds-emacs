;;; mds-sql-dsl.el --- Linguagem de Domínio Específico para Linguagem de Consulta Estruturada (Domain Specific Language for Structured Query Language) -*- lexical-binding: t -*-
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
  :bind
  (:map sql-mode-map
        ("C-c u" . sqlup-capitalize-keywords-in-region))
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package sqlup-mode
  :ensure t
  :commands sqlup-mode)

(use-package sql-indent
  :ensure t
  :after sql
  :config
  (with-eval-after-load 'sql (require 'sql-indent)))

;; https://github.com/kostafey/ejc-sql
(use-package ejc-sql
  :ensure t
  :commands ejc-sql-mode ejc-connect
  :config
  (ejc-create-connection
   "MySQL-PGD"
   :classpath (concat "~/.m2/repository/mysql/mysql-connector-java/5.1.6/"
                      "mysql-connector-java-5.1.6.jar")
   :classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//localhost:3306/picture_gallery_dev"
   :user "dummy"
   :password "dummy"))

(provide 'mds-sql-dsl)
;;; mds-sql-dsl.el ends here
