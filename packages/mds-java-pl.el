;;; mds-java-pl.el --- Linguagem de Programação Java (Java Programming Language)
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
;; Configurações para a linguagem Java.

;;; Code:
(use-package java-mode
  :mode
  ("\\.java$" . java-mode)
  :interpreter
  ("java" . java-mode)
  :init
  (use-package company
    :config
    (add-hook 'java-mode-hook (lambda ()
                                (set (make-local-variable 'company-transformers) '(company-sort-by-backend-importance company-sort-by-statistics))
                                (set (make-local-variable 'company-backends)
                                     '((company-meghanada
                                        company-keywords
                                        company-yasnippet
                                        company-abbrev
                                        company-dabbrev-code
                                        company-dabbrev
                                        company-dict
                                        company-files
                                        :with company-ispell))))))
  (use-package java-snippets
    :ensure t)
  (use-package flycheck
    :config
    (add-hook 'java-mode-hook 'flycheck-mode))
  (use-package jtags
    :ensure t
    :config
    (add-hook 'java-mode-hook 'jtags-mode))
  (use-package meghanada
    :ensure t
    :config
    (add-hook 'java-mode-hook (lambda ()
                                (meghanada-mode t)))))

(provide 'mds-java-pl)
;;; mds-java-pl ends here
