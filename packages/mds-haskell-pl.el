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
  :mode
  ("\\.hs$"      . haskell-mode)
  ("\\.lhs\\'"   . literate-haskell-mode)
  ("\\.hsc\\'"   . haskell-mode)
  ("\\.cpphs\\'" . haskell-mode)
  ("\\.c2hs\\'"  . haskell-mode)
  :bind
  (:map haskell-mode-map
        ("M-<right>" . haskell-move-nested-right)
        ("M-<left>"  . haskell-move-nested-left)
        ("C-c ."     . counsel-dash-at-point)
        ;; Editing
        ("C-c e j" . haskell-navigate-imports)
        ("C-c e f" . haskell-mode-format-imports)
        ("C-c e s" . haskell-sort-imports)
        ("C-c e a" . haskell-align-imports)
        ("C-c e S" . haskell-mode-stylish-haskell)
        ;; Compilation
        ("C-c c"   . haskell-compile)
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
        ;; Hare
        ("C-c h d m" . hare-refactor-demote)
        ("C-c h d d" . hare-refactor-dupdef)
        ("C-c h i c" . hare-refactor-iftocase)
        ("C-c h l o" . hare-refactor-lift-one)
        ("C-c h l t" . hare-refactor-lifttotop)
        ("C-c h r"   . hare-refactor-rename)
        ("C-c h t"   . hare-refactor-roundtrip)
        ("C-c h s h" . hare-refactor-show))
  :init
  (use-package haskell-snippets
    :ensure t
    :defer t)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-tags-on-save t
        haskell-stylish-on-save t
        haskell-notify-p t
        haskell-font-lock-symbols t
        company-ghc-show-info t)
  (add-hook 'haskell-mode-hook (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'haskell-mode-hook (lambda () (ghc-init) (hare-init)))
  (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (set (make-local-variable 'company-transformers)
                                      '(company-sort-by-backend-importance
                                        company-sort-prefer-same-case-prefix
                                        company-sort-by-statistics))
                                 (set (make-local-variable 'company-backends)
                                      '((company-capf
                                         company-ghc
                                         company-yasnippet
                                         :with
                                         company-abbrev
                                         company-dabbrev-code
                                         company-dabbrev
                                         company-files)))))
  (add-hook 'haskell-interactive-mode-hook (lambda ()
                                             (set (make-local-variable 'company-transformers)
                                                  '(company-sort-by-backend-importance
                                                    company-sort-prefer-same-case-prefix
                                                    company-sort-by-statistics))
                                             (set (make-local-variable 'company-backends)
                                                  '((company-capf
                                                     company-ghc
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
  ;; Which-key
  (mapcar
   (lambda (mode)
       (which-key-add-major-mode-key-based-replacements mode
         "C-c e" "editing"
         "C-c i" "interpreter"
         "C-c l" "lookup"
         "C-c h" "hare"))
   [haskell-mode literate-haskell-mode]))

(use-package ghc
  :ensure t
  :commands ghc-init ghc-debug)

(use-package company-ghc
  :ensure t
  :commands company-ghc)

(use-package shm
  :ensure t
  :commands structured-haskell-mode)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup
  :init
  (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(provide 'mds-haskell-pl)
;;; mds-haskell-pl ends here
