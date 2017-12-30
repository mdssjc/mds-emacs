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
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("<return>"  . nil)
        ("<tab>"     . company-select-next)
        ("<backtab>" . company-select-previous))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local company-frontends '(company-tng-frontend
                                              company-echo-metadata-frontend
                                              company-pseudo-tooltip-unless-just-one-frontend
                                              company-preview-if-just-one-frontend
                                              company-preview-common-frontend))
              (setq-local company-backends '((company-capf
                                              company-yasnippet
                                              company-abbrev
                                              company-dabbrev-code
                                              company-dabbrev
                                              company-files)))
              (setq-local company-transformers '(company-sort-prefer-same-case-prefix))
              (setq-local company-minimum-prefix-length 1)
              (setq-local company-search-regexp-function 'company-search-flex-regexp)))
  :config
  (setq company-lighter "ⓐ"
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-preview-common-frontend)
        company-backends '((company-ispell
                            company-abbrev
                            company-dabbrev
                            company-files))
        company-transformers '(company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence)
        company-minimum-prefix-length 2
        company-idle-delay 0
        company-occurrence-weight-function 'company-occurrence-prefer-any-closest
        company-search-regexp-function 'regexp-quote
        company-require-match nil
        company-show-numbers t
        company-selection-wrap-around t
        ;; Tooltip
        company-tooltip-limit 10
        company-tooltip-minimum 5
        company-tooltip-offset-display 'lines
        company-tooltip-align-annotations t
        company-tooltip-idle-delay 0
        ;; Dabbrev Code
        company-dabbrev-code-modes '(prog-mode)
        company-dabbrev-code-everywhere t
        ;; Dabbrev
        company-dabbrev-minimum-length 3
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-invisible t)
  ;; http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.

     Unless the number is potentially part of the candidate.
     In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(use-package company-quickhelp
  :ensure t
  :hook (prog-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay     0.25
        company-quickhelp-max-lines 30))

(use-package company-statistics
  :ensure t
  :commands company-statistics-mode
  :init
  (setq company-statistics-file (concat user-emacs-directory ".cache/company-statistics-cache.el"))
  :config
  (setq company-statistics-size 1000))

(use-package company-dict
  :ensure t
  :commands company-dict
  :config
  (setq company-dict-enable-fuzzy t))
;; ---

;; Correção (Correction)
(use-package ispell
  :ensure t
  :commands company-ispell
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
  (cond ((executable-find "hunspell")
         (setq ispell-program-name "hunspell"
               ispell-really-hunspell t))
        ((executable-find "aspell")
         (setq ispell-program-name "aspell"
               ispell-really-aspell t
               ispell-extra-args '("--sug-mode=ultra"))))
  (setq ispell-list-command "--list"
        ispell-look-p nil
        ispell-grep-command "rg"
        ispell-grep-options "-i"
        ispell-dictionary "pt_BR"
        ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/pt_BR.dic")
        ispell-choices-win-default-height 5
        ispell-lazy-highlight nil))
;; ---

;; Abreviação (Abbreviation)
(use-package abbrev
  :commands abbrev-mode
  :init
  (setq abbrev-file-name (concat user-emacs-directory "dict/abbrevs_defs.el"))
  (setq-default abbrev-mode t))
;; ---

;; Modelo (Template)
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode yas-global-mode
  :bind
  (:map yas-minor-mode-map
        ("C-c & w" . aya-create)
        ("C-c & y" . aya-expand)
        ("C-c & o" . aya-open-line))
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
  :hook (after-init . auto-insert-mode)
  :init
  (add-hook 'find-file-hook 'auto-insert)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (setq auto-insert-directory (concat user-emacs-directory "templates")
        auto-insert-query nil)
  (define-auto-insert "\\.c$"    ["template-c"      autoinsert-yas-expand])
  (define-auto-insert "\\.java$" ["template-java"   autoinsert-yas-expand])
  (define-auto-insert "\\.rkt$"  ["template-racket" autoinsert-yas-expand]))
;; ---

(provide 'mds-syntax)
;;; mds-syntax.el ends here
