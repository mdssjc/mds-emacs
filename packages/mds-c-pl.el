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
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (setq-local company-backends '((company-irony-c-headers
                                              company-irony
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
              ;;(electric-spacing-mode)
              (electric-pair-mode)
              (show-paren-mode)
              (flycheck-mode)
              (irony-mode)))
  (add-hook 'irony-mode-hook
            (lambda ()
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
  :commands irony-mode
  :bind
  (:map irony-mode-map
        ("C-c C-r s" . srefactor-refactor-at-point)
        (";"         . maio/electric-semicolon))
  :config
  (setq irony-user-dir (concat user-emacs-directory ".cache/irony"))
  (which-key-add-major-mode-key-based-replacements 'irony-mode-map
    "C-c ,"   "semantic"
    "C-c @"   "hide blocks"
    "C-c C-r" "refactor"))

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

(provide 'mds-c-pl)
;;; mds-c-pl.el ends here
