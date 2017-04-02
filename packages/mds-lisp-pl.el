;;; mds-lisp-pl.el --- Linguagem de Programação Lisp (Lisp Programming Language)
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
;; Configurações para a linguagem Lisp, nos dialetos:
;; ELisp (Emacs Lisp), Racket, Clojure e LFE.

;;; Code:
(use-package emacs-lisp-mode
  :mode
  ("\\.el$" . emacs-lisp-mode)
  :interpreter
  ("emacs" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (parinfer-mode)
               (prettify-symbols-mode)
               (eldoc-mode)
               (emr-initialize)
               (erefactor-lazy-highlight-turn-on)
               (require 'ert)
               (ert--activate-font-lock-keywords)
               (add-to-list 'completion-styles 'initials t)
               (setq-local company-backends '((company-elisp
                                               company-yasnippet
                                               :with
                                               company-capf
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))
               (flycheck-mode)
               (eval-after-load 'flycheck '(flycheck-package-setup))
               (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        lisp-prettify-symbols-alist   '(("lambda" . ?λ)
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
  (add-hook 'racket-mode-hook
            '(lambda ()
               (parinfer-mode)
               (prettify-symbols-mode)
               (dr-racket-like-unicode-mode)
               ;; (racket-unicode-input-method-enable)
               (setq-local company-backends '((company-capf
                                               :with
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))
               (flycheck-mode)
               (setq-local counsel-dash-docsets '("Racket"))))
  (add-hook 'racket-repl-mode-hook 'racket-unicode-input-method-enable)
  :config
  (setq racket-smart-open-bracket-enable t
        lisp-prettify-symbols-alist '(("lambda" . ?λ)
                                      ("->"     . ?→)
                                      ("=>"     . ?⇒)
                                      ("map"    . ?↦)
                                      ("."      . ?•))
        prettify-symbols-alist lisp-prettify-symbols-alist
        prettify-symbols-unprettify-at-point 'right-edge))

(use-package clojure-mode
  :ensure t
  :mode
  (("\\.clj\\'"  . clojure-mode)
   ("\\.edn\\'"  . clojure-mode)
   ("\\.cljc\\'" . clojurec-mode)
   ("\\.cljx\\'" . clojurex-mode))
  :init
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (parinfer-mode)
               (cider-mode)
               (subword-mode)
               (eldoc-mode)
               (setq-local company-backends '((company-capf
                                               company-yasnippet
                                               :with
                                               company-abbrev
                                               company-dabbrev-code
                                               company-dabbrev
                                               company-files)))
               (setq-local counsel-dash-docsets '("Clojure")))))

(use-package lfe-mode
  :ensure t
  :mode
  ("\\.lfe\\(s\\|sh\\)?\\'" . lfe-mode)
  :init
  (add-hook 'lfe-mode-hook
            '() (parinfer-mode)))

;; General
(use-package parinfer
  :ensure t
  :commands parinfer-mode parinfer-toggle-mode
  :config
  (setq parinfer-extensions '(defaults pretty-parens smart-yank smart-tab paredit lispy one)
        parinfer-preview-cursor-scope t
        parinfer-lighters '(" P:>>" . "P:()")))

(use-package paredit
  :ensure t
  :commands paredit-mode enable-paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :commands lispy-mode
  :init
  (add-hook 'minibuffer-setup-hook '(lambda () (when (eq this-command 'eval-expression)
                                            (lispy-mode 1)))))

(use-package litable
  :ensure t
  :commands litable-mode
  :diminish litable-mode " Ⓣ")

(use-package flycheck-package
  :ensure t
  :commands flycheck-package-setup)

(use-package emr
  :ensure t
  :commands emr-initialize emr-show-refactor-menu)

(use-package erefactor
  :ensure t
  :commands erefactor-lazy-highlight-turn-on
  :config
  (define-key emacs-lisp-mode-map "\C-crv" erefactor-map))
;; ---

;; Racket
(use-package dr-racket-like-unicode
  :ensure t
  :commands dr-racket-like-unicode-mode)
;; ---

;; Clojure
(use-package cider
  :ensure t
  :commands cider-mode
  :init
  (add-hook 'cider-mode-hook
            '(lambda ()
               (clj-refactor-mode)
               (cider-hydra-on)))
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
            '(lambda ()
               (parinfer-mode)
               (eldoc-mode)))
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-history-file (concat user-emacs-directory ".cache/cider/history-repl")
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :ensure t
  :after clojure-mode
  :init
  (with-eval-after-load 'eval-sexp-fu (require 'cider-eval-sexp-fu)))

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode
  :diminish clj-refactor-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider-hydra
  :ensure t
  :commands cider-hydra-on)
;; ---

(provide 'mds-lisp-pl)
;;; mds-lisp-pl.el ends here
