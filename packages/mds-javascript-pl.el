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
  :bind
  (:map js-mode-map
        ("M-." . nil)
        :map js2-mode-map
        (";" . maio/electric-semicolon))
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (lsp-javascript-typescript-enable)
              (js2-imenu-extras-mode)
              (js2-refactor-mode)
              (js-auto-beautify-mode)
              (electric-pair-mode)
              (show-paren-mode)
              (add-hook 'xref-backend-functions 'xref-js2-xref-backend nil t)
              (setq-local company-backends '((company-lsp
                                              company-abbrev
                                              company-dabbrev-code
                                              company-dabbrev
                                              company-files)))))
  :config
  (setq js2-highlight-level 3
        js-indent-level 2))

(use-package lsp-javascript-typescript
  :ensure t
  :commands lsp-javascript-typescript-enable)

(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :ensure t
  :commands xref-js2-xref-backend)

(use-package js-auto-beautify
  :ensure t
  :commands js-auto-beautify-mode)

(provide 'mds-javascript-pl)
;;; mds-javascript-pl.el ends here
