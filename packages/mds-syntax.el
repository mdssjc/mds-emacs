;;; mds-syntax.el --- Sintaxe (Syntax)
;;
;; Copyright (C) 2016-2016 Marcelo dos Santos
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
  :commands company-mode
  :defines
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  company-dabbrev-minimum-length
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-preview-common-frontend)
        company-tooltip-limit 10
        company-tooltip-minimum 5
        company-tooltip-offset-display 'lines
        company-tooltip-flip-when-above t
        company-backends '((company-abbrev
                            company-dabbrev
                            company-dict
                            company-files))
        company-transformers '(company-sort-by-occurrence)
        company-minimum-prefix-length 0
        company-idle-delay 0.1
        company-tooltip-idle-delay 0.1
        company-show-numbers t
        company-occurrence-weight-function 'company-occurrence-prefer-any-closest
        company-search-regexp-function 'company-search-flex-regexp
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-minimum-length 2))

(use-package company-quickhelp
  :ensure t
  :commands company-quickhelp-mode
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay .25
        company-quickhelp-max-lines 30))

(use-package company-statistics
  :ensure t
  :commands company-statistics-mode
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode)
  :config
  (run-with-idle-timer (* 60 3) t 'company-statistics--save)
  (setq company-statistics-size 500
        company-statistics-file (concat user-emacs-directory ".cache/company-statistics-cache.el")
        company-statistics-auto-save nil))

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
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pt_BR"
        ispell-really-hunspell t
        ispell-complete-word-dict (concat (expand-file-name user-emacs-directory) "dict/pt_BR.dic")
        ispell-choices-win-default-height 5))
;; ---

;; Abreviação (Abbreviation)
(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq-default abbrev-mode t)
  :config
  (setq save-abbrevs 'silently
        abbrev-file-name (concat user-emacs-directory "dict/abbrevs_defs.el")))
;; ---

;; Modelo (Template)
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode " ⓨ"
  :commands yas-minor-mode yas-global-mode
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))
  (yas-reload-all))

(use-package auto-yasnippet
  :ensure t
  :commands aya-create aya-expand aya-open-line)
;; ---

(provide 'mds-syntax)
;;; mds-syntax.el ends here
