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
  :bind
  (:map java-mode-map
        ("<f6> m" . meghanada-mode)
        ("<f6> j" . jdee-mode))
  :init
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook (lambda () (setq-local counsel-dash-docsets '("Java_SE8"
                                                                     "Java_EE7"
                                                                     "JavaFX"))))
  (add-hook 'java-mode-hook '(lambda ()
                               (set (make-local-variable 'company-transformers)
                                    '(company-sort-by-backend-importance
                                      company-sort-prefer-same-case-prefix
                                      company-sort-by-statistics))
                               (set (make-local-variable 'company-backends)
                                    '((company-meghanada
                                       company-yasnippet
                                       :with
                                       company-keywords
                                       company-abbrev
                                       company-dabbrev-code
                                       company-dabbrev
                                       company-dict
                                       company-files))))))

(use-package java-snippets
  :ensure t
  :defer t)

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :init
  (add-hook 'java-mode-hook 'meghanada-mode)
  :config
  (setq meghanada-server-install-dir (concat user-emacs-directory ".cache/meghanada/")))

(use-package jdee
  :ensure t
  :commands jdee-mode
  :config
  (setq jdee-server-dir (concat user-emacs-directory ".cache/jdee/")))

(provide 'mds-java-pl)
;;; mds-java-pl.el ends here
