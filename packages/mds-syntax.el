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
;;  - Template: blocos de construção de texto (linguagens de programação).

;;; Code:
(use-package company
  :ensure t
  :diminish company-mode " ⓐ"
  :bind
  (("<f5> a" . company-mode))
  :init
  (setq company-tooltip-limit 10
        company-tooltip-minimum 5
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-numbers t
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function 'company-search-words-regexp
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix
                               company-sort-by-statistics)
        company-backends '((company-abbrev
                            company-dabbrev
                            company-dict
                            company-files
                            company-math-symbols-unicode
                            company-emoji
                            :with
                            company-ispell)))
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :init
  (setq company-quickhelp-delay 1
        company-quickhelp-max-lines 30)
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package company-statistics
  :ensure t
  :init
  (setq company-statistics-size 2500
        company-statistics-file (concat user-emacs-directory
                                        ".cache/company-statistics-cache.el")
        company-statistics-auto-save nil)
  (add-hook 'company-mode-hook 'company-statistics-mode)
  :config
  (add-hook 'auto-save-hook 'company-statistics--maybe-save))

(use-package company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat user-emacs-directory
                                 "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet t))

(use-package company-emoji
  :ensure t
  :after company
  :config
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
  (company-emoji-init))

(use-package company-math
  :ensure t
  :after company)

;; Correção (Correction)
(use-package ispell
  :ensure t
  :preface
  (defun ispell-pt-br ()
    (interactive)
    (setq ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/pt_BR.dic")
    (ispell-change-dictionary "pt_BR"))
  (defun ispell-en-us ()
    (interactive)
    (setq ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/en_US.dic")
    (ispell-change-dictionary "en_US"))
  (defun ispell-en-gb ()
    (interactive)
    (setq ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/en_GB.dic")
    (ispell-change-dictionary "en_GB"))
  :bind
  (("<f8> s p" . ispell-pt-br)
   ("<f8> s e" . ispell-en-us)
   ("<f8> s g" . ispell-en-gb))
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pt_BR"
        ispell-really-hunspell t
        ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/pt_BR.dic"))

;; Abreviação (Abbreviation)
(use-package abbrev
  :diminish abbrev-mode
  :init
  (add-hook 'after-init-hook 'abbrev-mode)
  :config
  (setq save-abbrevs 'silently
        abbrev-file-name (concat user-emacs-directory
                                 "dict/abbrevs_defs.el")))

;; Template
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode " ⓨ"
  :commands yas-global-mode yas-minor-mode
  :bind
  (("<f5> y" . yas-global-mode))
  :init
  (setq yas-prompt-functions '(yas-x-prompt
                               yas-dropdown-prompt))
  :config
  (yas-reload-all))

(use-package auto-yasnippet
  :ensure t
  :bind
  (("C-c & w" . aya-create)
   ("C-c & y" . aya-expand)
   ("C-c & o" . aya-open-line)))

(provide 'mds-syntax)
;;; mds-syntax.el ends here
