;;; mds-javascript-pl.el --- Linguagem de Programação JavaScript (JavaScript Programming Language) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Configurações para a linguagem JavaScript.

;;; Code:
(use-package js2-mode
  :ensure t
  :mode
  (("\\.js\\'" . js2-mode))
  :interpreter
  ("node" . j2-mode)
  :init
  (add-hook 'js2-mode-hook
            '(lambda ()
               (js2-imenu-extras-mode)
               (js2-refactor-mode)
               ;;(tern-mode t)
               (lsp-mode)
               (js-auto-beautify-mode)
               (electric-pair-mode)
               (show-paren-mode)
               (add-hook 'xref-backend-functions 'xref-js2-xref-backend nil t)
               (setq-local company-transformers '(company-sort-prefer-same-case-prefix))
               (setq-local company-minimum-prefix-length 1)
               (setq-local company-idle-delay 0)
               (setq-local company-backends '((
                                               ;;company-tern
                                               company-lsp
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))))
  :config
  (setq js2-highlight-level 3
        js-indent-level 2))

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :ensure t
  :after js2-mode)

(use-package tern
  :ensure t
  :commands tern-mode
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package company-tern
  :ensure t
  :commands company-tern
  :config
  (setq company-tern-property-marker nil))

(use-package js-auto-beautify
  :ensure t
  :commands js-auto-beautify-mode)

(use-package lsp-javascript-typescript
  :ensure t
  :after js2-mode)

(provide 'mds-javascript-pl)
;;; mds-javascript-pl.el ends here
