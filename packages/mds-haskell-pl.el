;;; mds-haskell-pl.el --- Linguagem de Programação Haskell (Haskell Programming Language)
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
;; Configurações para a linguagem Haskell.

;;; Code:
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :bind
  (;; ("C-c 8"       . haskell-navigate-imports)
   ;; ("C-c C-c"     . haskell-compile)
   )
  :init
  (use-package haskell-snippets
    :ensure t
    :defer t)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook '(lambda () (ghc-init)))
  :config
  (setq haskell-tags-on-save t
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-stylish-on-save t)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  )

;; Travando
(use-package intero
  :ensure t
  :disabled t
  :commands intero-mode
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :ensure t
  :defer t)

(use-package ghc
  :ensure t
  :defer t)

(use-package flycheck-haskell
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'flycheck-haskell-setup))

(use-package flycheck
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package company-cabal
  :ensure t
  :defer t)

(use-package company-ghc
  :ensure t
  :defer t
  :config
  (setq company-ghc-show-info t))

(use-package company-ghci
  :ensure t
  :defer t)

(use-package company
  :defer t
  :init
  (add-hook 'haskell-mode-hook (lambda ()
                                 (set (make-local-variable 'company-transformers)
                                      '(company-sort-by-backend-importance
                                        company-sort-by-statistics))
                                 (set (make-local-variable 'company-backends)
                                      '((
                                         company-capf
                                         company-ghc
                                         company-ghci
                                         company-cabal
                                         company-keywords
                                         company-yasnippet
                                         company-abbrev
                                         company-dabbrev-code
                                         company-dabbrev
                                         company-dict
                                         company-files
                                         :with company-ispell)))))
  (add-hook 'haskell-interactive-mode-hook (lambda ())
                                 (set (make-local-variable 'company-transformers)
                                      '(company-sort-by-backend-importance
                                        company-sort-by-statistics))
                                 (set (make-local-variable 'company-backends)
                                      '((
                                         company-capf
                                         company-ghc
                                         company-ghci
                                         company-cabal
                                         company-keywords
                                         company-yasnippet
                                         company-abbrev
                                         company-dabbrev-code
                                         company-dabbrev
                                         company-dict
                                         company-files)))))

(use-package counsel-dash
  :defer t
  :init
  (add-hook 'haskell-mode-hook (lambda () (setq-local counsel-dash-docsets '("Haskell")))))

(provide 'mds-haskell-pl)
;;; mds-haskell-pl ends here
