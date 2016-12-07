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
  :bind
  (:map c-mode-map
        ("C-c j" . semantic-ia-fast-jump)
        ("C-c ^" . senator-go-to-up-reference)
        ("<M-return>" . srefactor-refactor-at-point))
  :init
  (setq c-default-style "linux"
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
                                    global-semantic-idle-local-symbol-highlight-mode
                                    global-semantic-highlight-edits-mode
                                    global-semantic-show-parser-state-mode
                                    global-semantic-idle-breadcrumbs-mode
                                    global-semantic-show-unmatched-syntax-mode))
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
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c-mode-hook 'fa-config-default)
  (add-hook 'c-mode-hook 'ede-enable-generic-projects)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  ;; (add-hook 'c-mode-hook 'electric-spacing-mode)
  (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
  :config
  (require 'semantic)
  (require 'semantic/ia)
  (require 'semantic/db)
  (require 'semantic/sb)
  (require 'semantic/bovine)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.2.1")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/lib/gcc/x86_64-pc-linux-gnu/6.2.1/include/stddef.h")
  (semanticdb-enable-gnu-global-databases 'c-mode t))

(use-package company-c-headers
  :ensure t)

(use-package srefactor
  :ensure t)

(use-package function-args
  :ensure t)

(use-package c-eldoc
  :ensure t)

(provide 'mds-c-pl)
;;; mds-c-pl.el ends here
