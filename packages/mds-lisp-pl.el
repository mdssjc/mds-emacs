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
(use-package parinfer
  :ensure t
  :commands parinfer-mode
  :bind
  (:map parinfer-mode-map
        ("C-c <return>" . parinfer-toggle-mode))
  :config
  (setq parinfer-extensions '(defaults pretty-parens lispy smart-tab smart-yank one paredit)
        parinfer-auto-switch-indent-mode t
        parinfer-auto-switch-indent-mode-when-closing t
        parinfer-preview-cursor-scope t))

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :commands lispy-mode
  :init
  (add-hook 'parinfer-mode-enable-hook 'lispy-mode)
  (add-hook 'minibuffer-setup-hook '(lambda () (when (eq this-command 'eval-expression)
                                            (lispy-mode 1)))))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

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
        ("C-c e"  . macrostep-expand)
        ("C-c r e" . emr-show-refactor-menu))
  :init
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook '(lambda () (add-to-list 'completion-styles 'initials t)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (set (make-local-variable 'company-backends)
                                              '((company-elisp
                                                 company-yasnippet
                                                 :with
                                                 company-capf
                                                 company-abbrev
                                                 company-dabbrev-code
                                                 company-dabbrev
                                                 company-files)))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (progn (flycheck-mode)
                                                (eval-after-load 'flycheck
                                                  '(flycheck-package-setup)))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (progn (require 'ert)
                                                (ert--activate-font-lock-keywords))))
  (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        lisp-prettify-symbols-alist '(("lambda" . ?λ)
                                      ("->"     . ?→)
                                      ("=>"     . ?⇒)
                                      ("map"    . ?↦)
                                      ("."      . ?•))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge))

(use-package litable
  :ensure t
  :commands litable-mode
  :diminish litable-mode " Ⓣ")

(use-package flycheck-package
  :ensure t
  :commands flycheck-package-setup)

(use-package erefactor
  :ensure t
  :defer t
  :config
  (define-key emacs-lisp-mode-map "\C-crv" erefactor-map))

(use-package racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'" . racket-mode)
  :interpreter
  ("racket" . racket-mode)
  :bind
  (:map racket-mode-map
        ("<f5>"     . nil)
        ("M-C-<f5>" . nil)
        ("C-<f5>"   . nil)
        ("C-c c"    . racket-run-and-switch-to-repl)
        ("C-c C-s"  . racket-racket))
  :init
  (add-hook 'racket-mode-hook 'parinfer-mode)
  (add-hook 'racket-mode-hook 'prettify-symbols-mode)
  (add-hook 'racket-mode-hook 'dr-racket-like-unicode-mode)
  (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook 'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook '(lambda () (setq-local company-backends '((company-capf
                                                                     :with
                                                                     company-abbrev
                                                                     company-dabbrev-code
                                                                     company-dabbrev
                                                                     company-files)))))
  (add-hook 'racket-mode-hook '(lambda () (setq-local counsel-dash-docsets '("Racket"))))
  :config
  (setq racket-smart-open-bracket-enable t
        lisp-prettify-symbols-alist '(("lambda" . ?λ)
                                      ("->"     . ?→)
                                      ("=>"     . ?⇒)
                                      ("map"    . ?↦)
                                      ("."      . ?•))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge))

(use-package dr-racket-like-unicode
  :ensure t
  :commands dr-racket-like-unicode-mode)

(use-package clojure-mode
  :ensure t
  :mode
  ("\\.cjr\\'" . clojure-mode)
  :init
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook '(lambda () (setq-local counsel-dash-docsets '("Clojure"))))
  (add-hook 'clojure-mode-hook '(lambda () (set (make-local-variable 'company-backends)
                                                '((company-capf
                                                   company-yasnippet
                                                   :with
                                                   company-abbrev
                                                   company-dabbrev-code
                                                   company-dabbrev
                                                   company-files)))))
  (add-hook 'clojure-mode-hook 'flyspell-prog-mode))

(use-package cider
  :ensure t
  :after clojure-mode)

(provide 'mds-lisp-pl)
;;; mds-lisp-pl.el ends here
