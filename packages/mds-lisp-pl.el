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

(setq lisp-prettify-symbols-alist
      '(("lambda" . ?λ)
        ("->" . ?→)
        ("=>" . ?⇒)
        ("map" . ?↦)))

(defun config-common ()
  "Configurações comum entre os dialetos."
  (require 'lispy)
  (require 'rainbow-delimiters)

  (setq prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge)

  (lispy-mode t)
  (rainbow-delimiters-mode t)
  (prettify-symbols-mode t))

(use-package emacs-lisp-mode
  :interpreter (("emacs" . emacs-lisp-mode))
  :bind
  (("M-." . find-function-at-point)
   ("M-&" . complete-symbol))
  :init
  (use-package macrostep :bind ("C-c e" . macrostep-expand))
  (use-package eldoc :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  (use-package ert :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))
  (use-package company
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda ()
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
  (use-package flycheck
    :config
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
    (setq flycheck-emacs-lisp-load-path 'inherit))
  (use-package litable
    :ensure t
    :diminish litable-mode " Ⓣ"
    :bind (("<f6> l" . litable-mode)))
  (add-to-list 'completion-styles 'initials t)
  (add-hook 'emacs-lisp-mode-hook 'config-common))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :init (add-hook 'racket-mode-hook 'config-common))

(use-package lispy
  :ensure t
  :diminish lispy-mode " Ⓛ"
  :bind
  (("<f6> l" . lispy-mode)
   :map lispy-mode-map
   ("s-<right>" . lispy-forward-slurp-sexp)
   ("S-s-<right>" . lispy-forward-barf-sexp)
   ("s-<left>" . lispy-backward-slurp-sexp)
   ("S-s-<left>" . lispy-backward-barf-sexp)))

(use-package geiser
  :ensure t
  :bind
  (:map racket-mode-map
        ("C-c C-c" . geiser-eval-last-sexp))
  :init
  (setq geiser-racket-binary "/usr/bin/racket"))

(use-package rainbow-delimiters :ensure t)

(provide 'mds-lisp-pl)
;;; mds-lisp-pl ends here
