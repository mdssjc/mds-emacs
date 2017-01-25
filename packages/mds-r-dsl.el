;;; mds-r-dsl.el --- Linguagem de Domínio Específico (Domain Specific Language)
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
;; Configurações para a linguagem R.

;;; Code:
(use-package ess
  :ensure t
  :defer 5
  :config
  (add-to-list 'load-path "~/Documents/Git/ESS/lisp/")
  (load "ess-site"))

(provide 'mds-r-dsl)
;;; mds-r-dsl.el ends here
