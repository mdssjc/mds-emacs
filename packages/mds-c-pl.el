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
        ("C-c r e" . emr-show-refactor-menu)
        ("C-c r s" . srefactor-refactor-at-point)
        (";"       . maio/electric-semicolon))
  :init
  (setq irony-user-dir (concat user-emacs-directory ".cache/irony"))
  (add-hook 'c-mode-hook '(lambda ()
                            (setq-local company-transformers
                                        '(company-sort-by-backend-importance
                                          company-sort-prefer-same-case-prefix
                                          company-sort-by-statistics))
                            (setq-local company-backends '((company-irony
                                                            company-irony-c-headers
                                                            :with
                                                            company-yasnippet
                                                            company-abbrev
                                                            company-dabbrev-code
                                                            company-dabbrev
                                                            company-files)))))
  (add-hook 'c-mode-hook 'flycheck-mode)
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
  (add-hook 'c-mode-hook 'function-args-mode)
  (add-hook 'c-mode-hook 'fa-config-default)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'electric-spacing-mode)
  (add-hook 'c-mode-hook 'electric-pair-mode)
  (add-hook 'c-mode-hook 'show-paren-mode)
  (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
  :config
  (setq company-backends (remove 'company-clang company-backends)
        c-default-style "linux"
        speedbar-show-unknow-files t))

(use-package irony
  :ensure t
  :commands irony-mode)

(use-package company-irony
  :ensure t
  :commands company-irony-setup-begin-commands company-irony)

(use-package company-irony-c-headers
  :ensure t
  :commands company-irony-c-headers)

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

(defun maio/electric-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")))

(provide 'mds-c-pl)
;;; mds-c-pl.el ends here
