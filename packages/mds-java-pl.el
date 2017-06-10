;;; mds-java-pl.el --- Linguagem de Programação Java (Java Programming Language) -*- lexical-binding: t -*-
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
;; Configurações para a linguagem Java.

;;; Code:
(use-package java-mode
  :mode
  (("\\.java$" . java-mode))
  :interpreter
  ("java" . java-mode)
  :init
  (add-hook 'java-mode-hook
            '(lambda ()
               (flycheck-mode)
               ;;(meghanada-mode)
               (setq-local counsel-dash-docsets '("Java_SE8" "Java_EE7" "JavaFX"))))
  (add-hook 'meghanada-mode-hook
            '(lambda ()
               (setq company-meghanada-prefix-length 1
                     company-occurrence-weight-function 'company-occurrence-prefer-any-closest)
               (setq-local company-transformers '(company-sort-prefer-same-case-prefix
                                                  company-sort-by-backend-importance
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-meghanada
                                               company-yasnippet
                                               :with
                                               company-keywords
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-dict
                                               company-files)))))
  (add-hook 'jdee-mode-hook
            '(lambda ()
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-yasnippet
                                               :with
                                               company-keywords
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-dict
                                               company-files))))))

(use-package java-snippets
  :ensure t
  :after java-mode)

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :init
  (setq meghanada-server-install-dir (concat user-emacs-directory ".cache/meghanada/")))

(use-package jdee
  :ensure t
  :commands jdee-mode
  :init
  (setq jdee-server-dir (concat user-emacs-directory ".cache/jdee/"))
  :config
  (require 'jdee-checkstyle))

(provide 'mds-java-pl)
;;; mds-java-pl.el ends here
