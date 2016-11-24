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
  :diminish parinfer " Ⓟ"
  :bind
  (:map parinfer-mode-map
        ("C-c <return>" . parinfer-toggle-mode))
  :init
  (setq parinfer-extensions '(defaults pretty-parens lispy smart-tab smart-yank)
        parinfer-auto-switch-indent-mode t))

(use-package lispy
  :ensure t
  :commands lispy-mode
  :diminish lispy-mode " Ⓛ"
  :init
  (add-hook 'parinfer-mode-hook 'lispy-mode))

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
        ("C-c e"  . macrostep-expand))
  :init
  (require 'ert)
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook '(lambda () (setq lisp-prettify-symbols-alist
                                               '(("lambda" . ?λ)
                                                 ("->"     . ?→)
                                                 ("=>"     . ?⇒)
                                                 ("map"    . ?↦)
                                                 ("."      . ?•))
                                               prettify-symbols-alist lisp-prettify-symbols-alist
                                               prettify-symbols-unprettify-at-point 'right-edge)
                                     (prettify-symbols-mode)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (progn (setq flycheck-emacs-lisp-load-path 'inherit)
                                                (flycheck-mode))))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (add-to-list 'completion-styles 'initials t)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (set (make-local-variable 'company-backends)
                                                   '((company-elisp
                                                      company-yasnippet
                                                      :with
                                                      company-capf
                                                      company-keywords
                                                      company-abbrev
                                                      company-dabbrev-code
                                                      company-dabbrev
                                                      company-files)))))
  (add-hook 'emacs-lisp-mode-hook 'flycheck-package-setup)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode))

(use-package litable
  :ensure t
  :commands litable-mode
  :diminish litable-mode " Ⓣ")

(use-package package-lint
  :ensure t)

(use-package flycheck-package
  :ensure t
  :after package-lint
  :commands flycheck-package-setup)

(use-package racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'" . racket-mode)
  :interpreter
  ("racket" . racket-mode)
  :init
  (add-hook 'racket-mode-hook 'parinfer-mode)
  (add-hook 'racket-mode-hook '(lambda () (setq lisp-prettify-symbols-alist
                                           '(("lambda" . ?λ)
                                             ("->"     . ?→)
                                             ("=>"     . ?⇒)
                                             ("map"    . ?↦)
                                             ("."      . ?•))
                                           prettify-symbols-alist lisp-prettify-symbols-alist
                                           prettify-symbols-unprettify-at-point 'right-edge)
                                 (prettify-symbols-mode)))
  (add-hook 'racket-mode-hook '(lambda () (setq-local counsel-dash-docsets '("Racket"))))
  (add-hook 'racket-mode-hook '(lambda () (set (make-local-variable 'company-backends)
                                          '((company-capf
                                             :with
                                             company-abbrev
                                             company-dabbrev-code
                                             company-dabbrev
                                             company-files)))))
  (add-hook 'racket-mode-hook 'flyspell-prog-mode)
  (add-hook 'racket-mode-hook 'dr-racket-like-unicode-mode)
  ;; (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook 'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook (lambda () (define-key racket-mode-map (kbd "<f5>") nil))))

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
;;; mds-lisp-pl ends here
