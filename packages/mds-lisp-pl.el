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
;; Configurações para a linguagem Lisp, nos dialetos: ELisp (Emacs Lisp), Racket e Clojure.

;;; Code:
(use-package lispy
  :ensure t
  :commands lispy-mode
  :diminish lispy-mode " Ⓛ"
  :init
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

(use-package litable
  :ensure t
  :commands litable-mode
  :diminish litable-mode " Ⓣ")

(use-package parinfer
  :ensure t
  :commands parinfer-mode
  :diminish parinfer " Ⓟ"
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (setq parinfer-extensions '(defaults
                              pretty-parens
                              lispy
                              smart-tab
                              smart-yank))
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook     'rainbow-delimiters-mode))

(use-package eldoc
  :ensure t
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package company
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (set (make-local-variable 'company-backends)
                                                  '((company-elisp
                                                     company-capf
                                                     company-keywords
                                                     company-yasnippet
                                                     company-abbrev
                                                     company-dabbrev-code
                                                     company-dabbrev
                                                     company-dict
                                                     company-files
                                                     :with company-ispell)))))
  (add-hook 'racket-mode-hook (lambda () (set (make-local-variable 'company-backends)
                                          '((company-capf
                                             company-keywords
                                             company-abbrev
                                             company-dabbrev-code
                                             company-dabbrev
                                             company-dict
                                             company-files
                                             :with company-ispell))))))

(use-package counsel-dash
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
  (add-hook 'racket-mode-hook     (lambda () (setq-local counsel-dash-docsets '("Racket")))))

(use-package ert
  :init
  (add-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))

(use-package yasnippet
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook '(lambda () (progn (yas-minor-mode)
                                                (yas-reload-all)))))

(use-package flycheck
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook '(lambda () (progn (setq flycheck-emacs-lisp-load-path 'inherit)
                                                (flycheck-mode)))))

(use-package flycheck-package
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-hode-hook 'flycheck-package-setup))

(use-package emacs-lisp-mode
  :mode
  ("\\.el$" . emacs-lisp-mode)
  :interpreter
  ("emacs" . emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("<f6> t" . litable-mode)
        ("M-."    . find-function-at-point)
        ("M-&"    . complete-symbol)
        ("C-c e"  . macrostep-expand))
  :init
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  :config
  (add-to-list 'completion-styles 'initials t)
  (setq lisp-prettify-symbols-alist '(("lambda" . ?λ)
                                      ("->"     . ?→)
                                      ("=>"     . ?⇒)
                                      ("map"    . ?↦)
                                      ("."      . ?•))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge))

(use-package racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'" . racket-mode)
  :interpreter
  ("racket" . racket-mode)
  :init
  (add-hook 'racket-mode-hook 'prettify-symbols-mode)
  :config
  (setq lisp-prettify-symbols-alist '(("lambda" . ?λ)
                                      ("->"     . ?→)
                                      ("=>"     . ?⇒)
                                      ("map"    . ?↦)
                                      ("."      . ?•))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge))

(use-package clojure-mode
  :ensure t
  :disabled t)

(provide 'mds-lisp-pl)
;;; mds-lisp-pl ends here
