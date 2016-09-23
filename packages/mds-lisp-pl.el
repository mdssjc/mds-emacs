;;; mds-lisp-pl.el --- Linguagem de Programação Lisp (Lisp Programming Language)
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
;; Dialeto ELisp (Emacs Lisp) e Racket

;;; Code:
(require 'semantic)

(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :bind
  (("M-." . find-function-at-point)
   ("M-&" . complete-symbol))
  :init
  (use-package macrostep :bind ("C-c e" . macrostep-expand))
  (use-package eldoc :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  (use-package lispy :config (add-hook 'emacs-lisp-mode-hook 'lispy-mode))
  (use-package ert :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))
  (use-package company
    :config
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (add-to-list (make-local-variable 'company-backends)
                             '(company-elisp
                               company-abbrev
                               company-dabbrev-code
                               company-dabbrev
                               company-keywords
                               company-files
                               company-capf
                               company-semantic
                               company-yasnippet
                               company-ispell)))))
  (use-package rainbow-delimiters
    :config
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
  (use-package flycheck
    :config
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
    (setq flycheck-emacs-lisp-load-path 'inherit))
  (use-package litable
    :ensure t
    :config (add-hook 'emacs-lisp-mode-hook 'litable-mode t))
  (add-to-list 'completion-styles 'initials t))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :init
  (use-package lispy :config (add-hook 'racket-mode-hook 'lispy-mode))
  (use-package rainbow-delimiters :config (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)))

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :bind
  (:map lispy-mode-map
        ("s-<right>" . lispy-forward-slurp-sexp)
        ("S-s-<right>" . lispy-forward-barf-sexp)
        ("s-<left>" . lispy-backward-slurp-sexp)
        ("S-s-<left>" . lispy-backward-barf-sexp)))

(use-package rainbow-delimiters :ensure t)

(provide 'mds-lisp-pl)
;;; mds-lisp-pl ends here
