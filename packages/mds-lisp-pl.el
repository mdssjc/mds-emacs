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
;; Configurações para os dialetos Lisp: ELisp (Emacs Lisp) e Racket

;;; Code:
;; (require 'semantic)

(defun config-common ()
  "Configurações comum entre os dialetos."
  (require 'lispy)
  (require 'rainbow-delimiters)

  (setq lisp-prettify-symbols-alist
        '(("lambda" . ?λ)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("map" . ?↦))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge)

  (lispy-mode t)
  (rainbow-delimiters-mode t)
  (prettify-symbols-mode t))

(use-package emacs-lisp-mode
  :defer t
  :mode
  ("\\.el$" . emacs-lisp-mode)
  :interpreter
  ("emacs" . emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("<f6> l" . lispy-mode)
        ("<f6> p" . parinfer-mode)
        ("<f6> t" . litable-mode)
        ("M-." . find-function-at-point)
        ("M-&" . complete-symbol))
  :init
  (use-package macrostep
    :bind
    (:map emacs-lisp-mode-map
          ("C-c e" . macrostep-expand)))
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  (use-package ert
    :config
    (add-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))
  (use-package company
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                      (add-to-list (make-local-variable 'company-backends)
                                                   '(company-elisp
                                                     company-capf
                                                     company-yasnippet
                                                     company-abbrev
                                                     company-dabbrev-code
                                                     company-dabbrev
                                                     company-keywords
                                                     company-files
                                                     company-ispell)))))
  (use-package flycheck
    :config
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))
  (add-to-list 'completion-styles 'initials t)
  (add-hook 'emacs-lisp-mode-hook 'config-common))

(use-package racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'" . racket-mode)
  :interpreter
  ("racket" . racket-mode)
  :bind
  (:map racket-mode-map
        ("<f6> l" . lispy-mode)
        ("<f6> g" . geiser-mode))
  :init
  (use-package company
    :config
    (add-hook 'racket-mode-hook (lambda ()
                                  (add-to-list (make-local-variable 'company-backends)
                                               '(company-capf
                                                 company-abbrev
                                                 company-dabbrev-code
                                                 company-dabbrev
                                                 company-keywords
                                                 company-files
                                                 company-ispell)))))
  (add-hook 'racket-mode-hook 'config-common))

(use-package lispy
  :ensure t
  :defer t
  :diminish lispy-mode " Ⓛ"
  :bind
  (:map lispy-mode-map
        ("s-<right>" . lispy-forward-slurp-sexp)
        ("S-s-<right>" . lispy-forward-barf-sexp)
        ("s-<left>" . lispy-backward-slurp-sexp)
        ("S-s-<left>" . lispy-backward-barf-sexp)))

(use-package litable
  :ensure t
  :defer t
  :diminish litable-mode " Ⓣ")

(use-package parinfer
  :ensure t
  :defer t
  :diminish parinfer " Ⓟ"
  :bind
  (:map parinfer-mode-map
        ("C-," . parinfer-toggle-mode)
        ("M-r" . parinfer-raise-sexp)
        ("M-m" . mark-sexp)
        ("M-j" . parinfer-transpose-sexps)
        ("M-k" . parinfer-reverse-transpose-sexps)))

(use-package geiser
  :ensure t
  :defer t
  :diminish geiser-mode " Ⓖ"
  :bind
  (:map geiser-mode-map
        ("C-c C-c" . geiser-eval-last-sexp))
  :config
  (setq geiser-racket-binary "/usr/bin/racket"))

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(provide 'mds-lisp-pl)
;;; mds-lisp-pl ends here
