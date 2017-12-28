;;; mds-lisp-pl.el --- Linguagem de Programação Lisp (Lisp Programming Language) -*- lexical-binding: t -*-
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
  :mode        "\\.el$"
  :interpreter "emacs"
  :hook (emacs-lisp-mode . emacs-lisp-mode-init)
  :bind
  (:map emacs-lisp-mode-map
        ("<f9> p" . parinfer-mode)
        ("<f9> P" . enable-paredit-mode)
        ("<f9> l" . lispy-mode)
        ("<f9> r" . rainbow-delimiters-mode)
        ("<f9> t" . litable-mode)
        ("M-&"    . complete-symbol)
        ("C-c e"  . macrostep-expand))
  :init
  (defun emacs-lisp-mode-init ()
    (parinfer-mode)
    (eldoc-mode)
    (require 'ert)
    (ert--activate-font-lock-keywords)
    (add-to-list 'completion-styles 'initials t)
    (company-statistics-mode)
    (setq-local company-backends '((company-elisp
                                    company-capf
                                    company-yasnippet
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-files)))
    (flycheck-mode)
    (flycheck-package-setup)
    (setq-local counsel-dash-docsets '("Emacs_Lisp")))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package racket-mode
  :ensure t
  :mode        "\\.rkt\\'"
  :interpreter "racket"
  :hook (racket-mode . racket-mode-init)
  :bind
  (:map racket-mode-map
        ("<f9> p"   . parinfer-mode)
        ("<f9> P"   . enable-paredit-mode)
        ("<f9> l"   . lispy-mode)
        ("<f9> r"   . rainbow-delimiters-mode)
        ("<f9> t"   . litable-mode)
        ("<f5>"     . nil)
        ("M-C-<f5>" . nil)
        ("C-<f5>"   . nil)
        ("C-c c"    . racket-run-and-switch-to-repl)
        ("C-c C-s"  . racket-racket))
  :init
  (add-hook 'racket-repl-mode-hook 'racket-unicode-input-method-enable)
  (defun racket-mode-init ()
    (parinfer-mode)
    (dr-racket-like-unicode-mode)
    (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                    company-preview-if-just-one-frontend
                                    company-preview-common-frontend))
    (setq-local company-backends '((company-capf
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-files)))
    (flycheck-mode)
    (setq-local counsel-dash-docsets '("Racket")))
  :config
  (setq racket-smart-open-bracket-enable t))

(use-package clojure-mode
  :ensure t
  :mode
  (("\\.\\(clj\\|dtm\\|edn\\)\\'"       . clojure-mode)
   ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
   ("\\.cljc\\'"                        . clojurec-mode)
   ("\\.cljx\\'"                        . clojurex-mode)
   ("\\.cljs\\'"                        . clojurescript-mode))
  :hook (clojure-mode . clojure-mode-init)
  :init
  (defun clojure-mode-init ()
    (parinfer-mode)
    (clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c C-r .")
    (clojure-font-lock-setup)
    (setq-local company-backends '((company-capf
                                    company-yasnippet
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-files)))
    (setq-local counsel-dash-docsets '("Clojure"))))

(use-package lfe-mode
  :ensure t
  :mode
  (("\\.lfe\\(s\\|sh\\)?\\'" . lfe-mode))
  :hook (lfe-mode . parinfer-mode))

;; General
(use-package parinfer
  :ensure t
  :defer t
  :hook (parinfer-mode-enable . parinfer-mode-init)
  :bind
  (:map parinfer-mode-map
        ("C-c <tab>" . parinfer-toggle-mode)
        ("C-1"       . lispy-describe-inline)
        ("C-2"       . lispy-arglist-inline)
        ("C-3"       . lispy-right)
        ("C-4"       . lispy-x)
        ("C-8"       . lispy-parens-down)
        ("b"         . special-lispy-back)
        ("t"         . special-lispy-teleport)
        ("/"         . special-lispy-splice)
        ("M-."       . lispy-goto-symbol)
        ("M-k"       . lispy-kill-sentence)
        ("C-+"       . lispy-mark)
        ("E"         . special-lispy-eval-and-insert)
        ("N"         . special-lispy-narrow)
        ("W"         . special-lispy-widen))
  :init
  (defun parinfer-mode-init ()
    (show-paren-mode)
    (electric-pair-mode)
    (push '("->"     . ?→) prettify-symbols-alist)
    (push '("=>"     . ?⇒) prettify-symbols-alist)
    (push '("map"    . ?↦) prettify-symbols-alist)
    (push '("."      . ?•) prettify-symbols-alist)
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (prettify-symbols-mode))
  :config
  (setq parinfer-extensions '(defaults pretty-parens smart-yank smart-tab paredit lispy one)
        parinfer-lighters '("𝓟:>>" . "𝓟:()")
        prettify-symbols-unprettify-at-point 'right-edge
        show-paren-style 'parenthesis
        show-paren-ring-bell-on-mismatch   t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :commands lispy-mode
  :init
  (add-hook 'minibuffer-setup-hook
            (lambda () (when (eq this-command 'eval-expression)
                    (lispy-mode 1)))))

(use-package paredit
  :ensure t
  :commands paredit-mode enable-paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

(use-package litable
  :ensure t
  :commands litable-mode
  :diminish litable-mode " Ⓣ"
  :config
  (setq litable-list-file (concat user-emacs-directory ".cache/.litable-lists.el")))

(use-package flycheck-package
  :ensure t
  :commands flycheck-package-setup)
;; ---

;; Racket
(use-package dr-racket-like-unicode
  :ensure t
  :commands dr-racket-like-unicode-mode)
;; ---

;; Clojure
(use-package cider
  :ensure t
  :commands cider cider-mode cider-connect cider-jack-in
  :init
  (add-hook 'cider-mode-hook
            (lambda ()
              (eldoc-mode)
              (subword-mode)))
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (eldoc-mode)
              (subword-mode)))
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
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
        cider-overlays-use-font-lock t
        cider-eval-result-prefix ";; => ")
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :ensure t
  :after cider)

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode
  :diminish clj-refactor-mode)

(use-package cider-hydra
  :ensure t
  :disabled t
  :commands cider-hydra-on)

(use-package clojure-snippets
  :ensure t
  :after clojure-mode)
;; ---

(provide 'mds-lisp-pl)
;;; mds-lisp-pl.el ends here
