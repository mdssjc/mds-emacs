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
;; Autocompletar (Autocomplete)
(use-package company
  :ensure t
  :diminish company-mode " ⓐ"
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-limit 10
        company-tooltip-minimum 5
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-numbers t
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-everywhere t
        company-search-regexp-function 'company-search-words-regexp
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix
                               company-sort-by-statistics)))

(use-package company-quickhelp
  :ensure t
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1
        company-quickhelp-max-lines 30))

(use-package company-statistics
  :ensure t
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode)
  :config
  (setq company-statistics-size 1000
        company-statistics-file (concat user-emacs-directory ".cache/company-statistics-cache.el")
        company-statistics-auto-save nil)
  (run-at-time nil 600 '(lambda () (async-start
                               (company-statistics--save)))))

(use-package company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet t))
;; ---

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
  :commands yas-minor-mode yas-global-mode
  :bind
  (:map yas-minor-mode-map
        ("C-&" . hydra-yasnippet/body))
  :init
  (setq yas-prompt-functions '(yas-x-prompt
                               yas-dropdown-prompt))
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :bind
  (:map yas-minor-mode-map
        ("C-c & w" . aya-create)
        ("C-c & y" . aya-expand)
        ("C-c & o" . aya-open-line)))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
             ^YASnippets^
--^------^---^-----------^---^--------^--
  ^Modes:^   ^Load/Visit:^   ^Actions:^
  _g_lobal   _d_irectory     _i_nsert
  _m_inor    _f_ile          _t_ryout
  _e_xtra    _l_ist          _n_ew
  ^ ^        _a_ll
---
Others:
"
  ("<ESC>" nil nil)
  ("q" nil nil)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("e" yas-activate-extra-mode)
  ("d" yas-load-directory)
  ("f" yas-visit-snippet-file :color blue)
  ("l" yas-describe-tables)
  ("a" yas-reload-all)
  ("i" yas-insert-snippet)
  ("t" yas-tryout-snippet)
  ("n" yas-new-snippet)
  ("c" aya-create    "aya-create")
  ("x" aya-expand    "aya-expand")
  ("o" aya-open-line "aya-open"))

(provide 'mds-syntax)
;;; mds-syntax.el ends here
