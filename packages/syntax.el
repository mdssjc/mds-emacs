;;
;; Syntax.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
;; Sintaxe (Syntax)
;; Autocompletar (Autocomplete)
(use-package company
  :ensure t
  :diminish company-mode
  :init
  :config
  (setq tab-always-indent 'complete
        company-tooltip-limit 10
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-auto-complete nil
        company-idle-delay 0.05
        company-show-numbers t
        company-echo-delay 0
        company-dabbrev-other-buffers 'all
        company-backends '((company-files
                            company-abbrev
                            company-dabbrev-code
                            company-dabbrev
                            company-keywords
                            company-capf
                            company-semantic
                            company-bbdb
                            company-etags
                            company-gtags
                            company-tempo
                            company-dict
                            company-yasnippet
                            company-ispell)))
  (global-company-mode t))
(use-package company-dict               ; company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil
        company-dict-dir (concat user-emacs-directory "dict/")))
(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode 1))
(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

;; Correção (Correction)
;; - instale o Hunspell
;; - entre com o comando Hunspell -D
;; - escolha a URI do dicionário com a extensão .dic
;; - entre com o pipe de comandos cat URI/XXX.dic | cut -d"/" -f1 > ~/.emacs.d/dicts/XXX
;; - converta a codificação do dicionário com o comando iconv -f ENCODE -t UTF-8 -o XXX
;; - configure a variável company-ispell-dictionary com o dicionário
;; - avançado: obtenha o dicionário pelo LanguageTool
(use-package ispell                     ; company-ispell
  :ensure t
  :after company
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pt_BR"
        ispell-really-hunspell t
        ispell-complete-word-dict "/home/mdssjc/.emacs.d/dict/pt_BR.dic"))

;; Abreviação (Abbreviation)

;; Template
(use-package yasnippet                  ; company-yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after company
  :config
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-completing-prompt
                               yas-no-prompt
                               yas-x-prompt))
  (yas-global-mode 1)
  (yas-reload-all))
