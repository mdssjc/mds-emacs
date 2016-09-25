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
;; FIXME -> o pacote está em testes

;;; Code:
(use-package java-mode
  :init
  (use-package meghanada
    :ensure t
    :init (add-hook 'java-mode-hook (lambda () (meghanada-mode t))))
  (use-package java-snippets :ensure t)
  (use-package company
    :config
    (add-hook 'java-mode-hook (lambda ()
                                (add-to-list (make-local-variable 'company-backends)
                                             '(company-meghanada
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-keywords
                                               company-files
                                               company-capf
                                               company-yasnippet
                                               company-ispell))))))

(provide 'mds-java-pl)
;;; mds-java-pl ends here
