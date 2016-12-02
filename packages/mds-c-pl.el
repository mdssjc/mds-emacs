;;; mds-c-pl.el --- Linguagem de Programação C (C Programming Language)
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
;; Configurações para a linguagem C.

;;; Code:
(use-package c-mode
  :ensure nil
  :mode
  ("\\.c$" . c-mode)
  ("\\.h$" . c-mode)
  :init
  (setq c-default-style "k&r"
        speedbar-show-unknow-files t
        semanticdb-default-save-directory (concat user-emacs-directory ".cache/semanticdb")
        semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-idle-completions-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-decoration-mode
                                    global-semantic-stickyfunc-mode
                                    global-semantic-mru-bookmark-mode
                                    global-semantic-idle-local-symbol-highlight-mode))
  (setq company-backends (remove 'company-clang company-backends))
  (add-hook 'c-mode-hook '(lambda ()
                            (set (make-local-variable 'company-transformers)
                                 '(company-sort-by-backend-importance
                                   company-sort-prefer-same-case-prefix
                                   company-sort-by-statistics))
                            (set (make-local-variable 'company-backends)
                                 '((company-semantic
                                    company-c-headers
                                    company-yasnippet
                                    :with
                                    company-keywords
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-files)))))
  (add-hook 'c-mode-hook '(lambda () (semantic-mode t)))
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
  ;(add-hook 'c-mode-hook 'electric-spacing-mode)
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.2.1"))

(use-package company-c-headers
  :ensure t)

(use-package srefactor
  :ensure t
  :bind
  (:map c-mode-map
   ("<M-return>" . srefactor-refactor-at-point)))

(provide 'mds-c-pl)
;;; mds-c-pl ends here
