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
  (use-package java-snippets
    :ensure t
    :defer t)
  (add-hook 'java-mode-hook 'yas-minor-mode)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'flyspell-prog-mode)
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
                                       company-files
                                       ;; company-ispell
                                       ))))))

(use-package meghanada
  :ensure t
  :defer t
  :bind
  (:map java-mode-map
        ("<f6> m" . meghanada-mode))
  :init
  (setq meghanada-server-install-dir (concat user-emacs-directory
                                             ".cache/meghanada/"))
  (add-hook 'java-mode-hook 'meghanada-mode))

(use-package jdee
  :ensure t
  :bind
  (:map java-mode-map
        ("<f6> j" . jdee-mode))
  :init
  (setq jdee-server-dir (concat user-emacs-directory
                                ".cache/jdee/")))

(provide 'mds-java-pl)
;;; mds-java-pl ends here
