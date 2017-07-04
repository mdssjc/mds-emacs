;;; mds-syntax.el --- Sintaxe (Syntax) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2016-2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense
;; Autocompletar (Autocomplete)

;;; Commentary:
;; Conjuntos de funcionalidades para facilitar a codificação e escrita.
;;  - Autocompletar: sugestão de palavras, conforme modo maior;
;;  - Correção: sugestão para palavras selecionadas, conforme dicionário (português por padrão);
;;  - Abreviação: abreviaturas de texto para expansão;
;;  - Modelo: blocos de construção de texto (linguagens de programação).

;;; Code:
;; Autocompletar (Autocomplete)
(use-package company
  :ensure t
  :diminish company-mode " ⓐ"
  :commands company-mode global-company-mode
  :defines
  company-dabbrev-code-modes
  company-dabbrev-code-everywhere
  company-dabbrev-code-ignore-case
  company-dabbrev-minimum-length
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  company-dabbrev-ignore-invisible
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'prog-mode-hook
            '(lambda ()
               (setq company-frontends '(company-echo-metadata-frontend
                                         company-pseudo-tooltip-unless-just-one-frontend
                                         company-preview-if-just-one-frontend
                                         company-preview-common-frontend))))
  (add-hook 'text-mode-hook
            '(lambda ()
               (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                         company-preview-if-just-one-frontend
                                         company-preview-common-frontend))))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-show-numbers t
        company-selection-wrap-around t
        company-backends '((company-capf company-abbrev company-dabbrev company-files company-ispell))
        company-transformers '(company-sort-prefer-same-case-prefix company-sort-by-occurrence)
        company-occurrence-weight-function 'company-occurrence-prefer-any-closest
        company-search-regexp-function 'company-search-flex-regexp
        company-require-match nil
        ;; Tooltip
        company-tooltip-limit 10
        company-tooltip-minimum 5
        company-tooltip-offset-display 'lines
        company-tooltip-align-annotations t
        company-tooltip-idle-delay 0
        ;; Dabbrev Code
        company-dabbrev-code-modes '(prog-mode)
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case nil
        ;; Dabbrev
        company-dabbrev-minimum-length 3
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-invisible t))

(use-package company-quickhelp
  :ensure t
  :commands company-quickhelp-mode
  :init
  (add-hook 'prog-mode-hook 'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay .25
        company-quickhelp-max-lines 30))

(use-package company-statistics
  :ensure t
  :commands company-statistics-mode
  :init
  (setq company-statistics-file (concat user-emacs-directory ".cache/company-statistics-cache.el"))
  (add-hook 'prog-mode-hook 'company-statistics-mode)
  :config
  ;;(run-with-idle-timer (* 60 3) t 'company-statistics--save)
  (setq company-statistics-size 1000
        company-statistics-auto-save t))

(use-package company-dict
  :ensure t
  :commands company-dict
  :config
  (setq company-dict-enable-fuzzy t))
;; ---

;; Correção (Correction)
(use-package ispell
  :ensure t
  :commands company-ispell ispell-pt-br ispell-en-us ispell-en-gb
  :defines
  ispell-list-command
  :preface
  (defun ispell-pt-br ()
    (interactive)
    (setq ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/pt_BR.dic"))
    (ispell-change-dictionary "pt_BR"))
  (defun ispell-en-us ()
    (interactive)
    (setq ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/en_US.dic"))
    (ispell-change-dictionary "en_US"))
  (defun ispell-en-gb ()
    (interactive)
    (setq ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/en_GB.dic"))
    (ispell-change-dictionary "en_GB"))
  :config
  (setq ispell-program-name "aspell"
        ispell-really-aspell t
        ispell-extra-args '("--sug-mode=ultra")
        ispell-list-command "--list"
        ispell-look-p nil
        ispell-grep-command "rg"
        ispell-grep-options ""
        ispell-dictionary "pt_BR"
        ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/pt_BR.dic")
        ispell-choices-win-default-height 5
        ispell-lazy-highlight nil))
;; ---

;; Abreviação (Abbreviation)
(use-package abbrev
  :diminish abbrev-mode
  :commands abbrev-mode
  :init
  (setq abbrev-file-name (concat user-emacs-directory "dict/abbrevs_defs.el"))
  (setq-default abbrev-mode t))
;; ---

;; Modelo (Template)
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode " ⓨ"
  :commands yas-minor-mode yas-global-mode
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook  'yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-completing-prompt yas-dropdown-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  (yas-reload-all))

(use-package auto-yasnippet
  :ensure t
  :commands aya-create aya-expand aya-open-line aya-persist-snippet
  :init
  (setq aya-persist-snippets-dir (concat user-emacs-directory ".cache/auto-snippets/")))

(use-package autoinsert
  :commands auto-insert-mode
  :init
  (add-hook 'after-init-hook 'auto-insert-mode)
  (add-hook 'find-file-hook  'auto-insert)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (setq auto-insert-directory (concat user-emacs-directory "templates")
        auto-insert-query nil)
  (define-auto-insert "\\.c$"    ["template-c" autoinsert-yas-expand])
  (define-auto-insert "\\.java$" ["template-java" autoinsert-yas-expand])
  (define-auto-insert "\\.rkt$"  ["template-racket" autoinsert-yas-expand]))
;; ---

(provide 'mds-syntax)
;;; mds-syntax.el ends here
