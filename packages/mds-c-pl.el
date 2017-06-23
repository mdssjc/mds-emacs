;;; mds-c-pl.el --- Linguagem de Programação C (C Programming Language) -*- lexical-binding: t -*-
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
;; Configurações para a linguagem C.

;;; Code:
(use-package cc-mode
  :mode
  (("\\.c$" . c-mode)
   ("\\.h$" . c-mode))
  :defines
  irony-user-dir
  speedbar-show-unknow-files
  :init
  (setq irony-user-dir (concat user-emacs-directory ".cache/irony"))
  (add-hook 'c-mode-hook
            '(lambda ()
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix))
               (setq-local company-backends '((company-irony-c-headers
                                               company-irony
                                               :with
                                               company-yasnippet
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))
               (setq company-backends (remove 'company-clang company-backends))
               (setq-local counsel-dash-docsets '("C"))
               (function-args-mode)
               (fa-config-default)
               (hs-minor-mode)
               (electric-spacing-mode)
               (electric-pair-mode)
               (show-paren-mode)
               (flycheck-mode)
               (irony-mode)))
  (add-hook 'irony-mode-hook
            '(lambda ()
               (company-irony-setup-begin-commands)
               (flycheck-irony-setup)
               (irony-eldoc)
               (define-key irony-mode-map [remap completion-at-point]
                 'irony-completion-at-point-async)
               (define-key irony-mode-map [remap complete-symbol]
                 'irony-completion-at-point-async)
               (irony-cdb-autosetup-compile-options)))
  :config
  (setq c-default-style "linux"
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
  "Insert a semicolon in expression."
  (interactive)
  (end-of-line)
  (when (not (looking-back ";" 0))
    (insert ";")))

(provide 'mds-c-pl)
;;; mds-c-pl.el ends here
