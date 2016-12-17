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
(use-package cc-mode
  :mode
  ("\\.c$" . c-mode)
  ("\\.h$" . c-mode)
  :bind
  (:map c-mode-map
        ("C-c ."      . semantic-ia-fast-jump)
        ("C-c ^"      . senator-go-to-up-reference)
        ("s-<return>" . srefactor-refactor-at-point)
        ("C-c C-r e"  . emr-show-refactor-menu)
        ("C-c C-r s"  . srefactor-refactor-at-point)
        (";"          . maio/electric-semicolon))
  :init
  (setq irony-user-dir (concat user-emacs-directory ".cache/irony"))
  (add-hook 'c-mode-hook '(lambda ()
                            (set (make-local-variable 'company-transformers)
                                 '(company-sort-by-backend-importance
                                   company-sort-prefer-same-case-prefix
                                   company-sort-by-statistics))
                            (set (make-local-variable 'company-backends)
                                 '((company-irony
                                    company-irony-c-headers
                                    company-semantic
                                    company-c-headers
                                    company-yasnippet
                                    :with
                                    company-keywords
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-files)))))
  (add-hook 'c-mode-hook 'flycheck-mode)
  ;; (add-hook 'c-mode-hook 'ede-enable-generic-projects)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'flycheck-irony-setup)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook '(lambda ()
                                (progn
                                  (define-key irony-mode-map [remap completion-at-point]
                                    'irony-completion-at-point-async)
                                  (define-key irony-mode-map [remap complete-symbol]
                                    'irony-completion-at-point-async))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c-mode-hook 'function-args-mode)
  (add-hook 'c-mode-hook 'fa-config-default)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'electric-spacing-mode)
  (add-hook 'c-mode-hook 'electric-pair-mode)
  (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
  :config
  (require 'semantic)
  (require 'semantic/ia)
  (require 'semantic/db)
  (require 'semantic/sb)
  (require 'semantic/bovine)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (setq company-backends (remove 'company-clang company-backends)
        c-default-style "linux"
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
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.2.1")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/lib/gcc/x86_64-pc-linux-gnu/6.2.1/include/stddef.h")
  (semanticdb-enable-gnu-global-databases 'c-mode t))

(use-package irony
  :ensure t
  :commands irony-mode)

(use-package company-irony
  :ensure t
  :commands company-irony-setup-begin-commands)

(use-package company-irony-c-headers
  :ensure t)

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup)

(use-package irony-eldoc
  :ensure t
  :commands irony-eldoc)

(use-package srefactor
  :ensure t
  :commands srefactor-refactor-at-point)

(use-package function-args
  :ensure t
  :diminish function-args-mode
  :commands function-args-mode fa-config-default)

(use-package company-c-headers
  :ensure t)

(defun maio/electric-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")))

(provide 'mds-c-pl)
;;; mds-c-pl.el ends here
