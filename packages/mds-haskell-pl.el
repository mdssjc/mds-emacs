;;; mds-haskell-pl.el --- Linguagem de Programação Haskell (Haskell Programming Language)
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
  ("\\.hs$"      . haskell-mode)
  ("\\.lhs\\'"   . literate-haskell-mode)
  ("\\.hsc\\'"   . haskell-mode)
  ("\\.cpphs\\'" . haskell-mode)
  ("\\.c2hs\\'"  . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (intero-mode)
               (intero-mode-whitelist)
               (intero-mode-blacklist)
               (hlint-refactor-mode)
               (haskell-decl-scan-mode)
               (flyckeck-haskell-configure)
               (turn-on-haskell-doc-mode)
               (haskell-indentation-mode)
               (electric-indent-local-mode -1)
               (rainbow-delimiters-mode)
               (speedbar-add-supported-extension ".hs")
               (lambda () (ghc-init) (hare-init))
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'intero-mode-hook
            '(lambda ()
               (setq-local company-backends '((company-intero
                                               company-yasnippet
                                               :with
                                               company-cabal
                                               company-ghci
                                               company-ghc
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))))
  (add-hook 'haskell-interactive-mode-hook
            '(lambda ()
               (setq-local company-transformers '(company-sort-by-backend-importance
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-ghci
                                               company-capf
                                               company-yasnippet
                                               :with
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))))
  :config
  ;; GHC
  (add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-8.0.1/ghc-mod-5.6.0.0/elisp")
  (autoload 'ghc-init  "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  ;; HaRe
  (add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-8.0.1/HaRe-0.8.3.0/elisp")
  (autoload 'hare-init "hare" nil t)
  ;; Variables
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-tags-on-save t
        haskell-stylish-on-save t
        haskell-notify-p t
        haskell-font-lock-symbols t
        company-ghc-show-info t)
  ;; Which-key
  (mapcar (lambda (mode) (which-key-add-major-mode-key-based-replacements mode
                      "C-c d" "debug"
                      "C-c e" "editing"
                      "C-c i" "interpreter"
                      "C-c l" "lookup"
                      "C-c r" "refactor"))
          [haskell-mode literate-haskell-mode]))

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package shm
  :ensure t
  :diminish structured-haskell-mode
  :commands structured-haskell-mode)

(use-package hlint-refactor
  :ensure t
  :diminish hlint-refactor-mode
  :commands hlint-refactor-mode)

(use-package haskell-snippets
  :ensure t
  :defer t)

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-configure)

(use-package ghc
  :ensure t
  :commands ghc-init ghc-debug)

(use-package company-cabal
  :ensure t
  :commands company-cabal)

(use-package company-ghci
  :ensure t
  :commands company-ghci)

(use-package company-ghc
  :ensure t
  :commands company-ghc)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

(provide 'mds-haskell-pl)
;;; mds-haskell-pl.el ends here
