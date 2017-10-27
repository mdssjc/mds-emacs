;;; mds-haskell-pl.el --- Linguagem de Programação Haskell (Haskell Programming Language) -*- lexical-binding: t -*-
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
;; Configurações para a linguagem Haskell.

;;; Code:
(use-package haskell-mode
  :ensure t
  :mode
  (("\\.hs$"      . haskell-mode)
   ("\\.lhs\\'"   . literate-haskell-mode)
   ("\\.hsc\\'"   . haskell-mode)
   ("\\.cpphs\\'" . haskell-mode)
   ("\\.c2hs\\'"  . haskell-mode))
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (lsp-haskell-enable)
              (hindent-mode)
              (haskell-doc-mode)
              (haskell-decl-scan-mode)
              (electric-pair-mode)
              (setq-local company-transformers '(company-sort-prefer-same-case-prefix))
              (setq-local company-minimum-prefix-length 1)
              (setq-local company-idle-delay 0)
              (setq-local company-backends '((company-lsp
                                              company-yasnippet
                                              company-abbrev
                                              company-dabbrev-code
                                              company-dabbrev
                                              company-files)))
              (flycheck-mode)
              (flycheck-haskell-setup)
              (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'haskell-interactive-mode-hook
            (lambda ()
              (setq-local company-backends '((company-ghci
                                              company-dabbrev-code
                                              company-yasnippet)))))
  :config
  (setq haskell-notify-p t
        haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-stylish-on-save nil)
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . haskell-modes)))))

(use-package lsp-haskell
  :ensure t
  :commands lsp-haskell-enable)

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package company-ghci
  :ensure t
  :commands company-ghci)

(use-package haskell-snippets
  :ensure t
  :after haskell-mode)

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup)

(use-package hindent
  :ensure t
  :commands hindent-mode)

(use-package hlint-refactor
  :ensure t
  :commands hlint-refactor-refactor-buffer hlint-refactor-refactor-at-point)

(provide 'mds-haskell-pl)
;;; mds-haskell-pl.el ends here
