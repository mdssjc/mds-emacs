;;; mds-java-pl.el --- Linguagem de Programação Java (Java Programming Language)
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
  ("\\.java$" . java-mode)
  :interpreter
  ("java" . java-mode)
  :init
  (add-hook 'java-mode-hook
            '(lambda () (progn
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
                                                     company-files)))
                     (flycheck-mode)
                     (setq-local counsel-dash-docsets '("Java_SE8" "Java_EE7" "JavaFX"))
                     (meghanada-mode)
                     (define-key java-mode-map (kbd "<f6> j") 'jdee-mode)
                     (define-key java-mode-map (kbd "<f6> m") 'meghanada-mode))))
  (add-hook 'jdee-mode-hook
            '(lambda () (progn
                     (setq-local company-backends '((company-yasnippet
                                                     :with
                                                     company-keywords
                                                     company-abbrev
                                                     company-dabbrev-code
                                                     company-dabbrev
                                                     company-dict
                                                     company-files))))))
  (add-hook 'meghanada-mode-hook
            '(lambda () (progn
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
                                                     company-files)))))))

(use-package java-snippets
  :ensure t
  :defer t)

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :config
  (setq meghanada-server-install-dir (concat user-emacs-directory ".cache/meghanada/")))

(use-package jdee
  :ensure t
  :commands jdee-mode
  :config
  (setq jdee-server-dir (concat user-emacs-directory ".cache/jdee/")))

(provide 'mds-java-pl)
;;; mds-java-pl.el ends here
