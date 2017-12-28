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
  :bind
  (:map haskell-mode-map
        ("M-<right>" . haskell-move-nested-right)
        ("M-<left>"  . haskell-move-nested-left)
        ("C-c ."     . counsel-dash-at-point)
        ("C-c f"     . hindent-reformat-decl)
        ("C-c SPC"   . lsp-apply-commands)
        ;; Debug
        ("C-c d a" . haskell-debug/abandon)
        ("C-c d b" . haskell-debug/break-on-function)
        ("C-c d B" . haskell-debug/delete)
        ("C-c d c" . haskell-debug/continue)
        ("C-c d d" . haskell-debug)
        ("C-c d n" . haskell-debug/next)
        ("C-c d N" . haskell-debug/previous)
        ("C-c d p" . haskell-debug/previous)
        ("C-c d r" . haskell-debug/refresh)
        ("C-c d s" . haskell-debug/step)
        ("C-c d t" . haskell-debug/trace)
        ;; Editing
        ("C-c e j" . haskell-navigate-imports)
        ("C-c e f" . haskell-mode-format-imports)
        ("C-c e s" . haskell-sort-imports)
        ("C-c e a" . haskell-align-imports)
        ("C-c e s" . haskell-mode-stylish-buffer)
        ("C-c e S" . haskell-mode-stylish-haskell)
        ;; Compilation
        ("C-c c" . haskell-compile)
        ;; Interpreter
        ("C-c '"   . haskell-interactive-bring)
        ("C-c i z" . switch-to-haskell)
        ("C-c i b" . switch-to-haskell)
        ("C-c i l" . inferior-haskell-load-file)
        ("C-c i t" . inferior-haskell-type)
        ("C-c i i" . inferior-haskell-info)
        ("C-c i d" . inferior-haskell-find-definition)
        ("C-c i c" . haskell-interactive-mode-clear)
        ;; Lookup
        ("C-c l t" . haskell-process-do-type)
        ("C-c l i" . haskell-process-do-info)
        ;; Refactor
        ("C-c r b" . hlint-refactor-refactor-buffer)
        ("C-c r r" . hlint-refactor-refactor-at-point)
        ;; Source
        ("C-c s b" . haskell-process-load-file)
        ("C-c s c" . haskell-interactive-mode-clear)
        ("C-c s s" . haskell-interactive-switch))
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              ;; (lsp-haskell-enable)
              (hindent-mode)
              (haskell-doc-mode)
              (haskell-decl-scan-mode)
              (electric-pair-mode)
              (setq-local company-backends '((;company-lsp
                                              company-yasnippet
                                              company-abbrev
                                              company-dabbrev-code
                                              company-dabbrev
                                              company-files)))
              (flycheck-mode)
              (flycheck-haskell-setup)
              (setq-local counsel-dash-docsets '("Haskell"))
              (which-key-add-major-mode-key-based-replacements 'haskell-mode
                "C-c d" "debug"
                "C-c e" "editing"
                "C-c i" "interpreter"
                "C-c l" "lookup")))
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
