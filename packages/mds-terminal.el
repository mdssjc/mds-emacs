;;; mds-terminal.el --- Terminal (Terminal) -*- lexical-binding: t -*-
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
;; Configurações para terminal.

;;; Code:
(use-package eshell
  :ensure nil
  :commands eshell
  :bind
  (:map shell-mode-map
        ("<tab>" . completion-at-point))
  :defines
  company-transformers
  company-backends
  eshell-history-size
  eshell-save-history-on-exit
  eshell-directory-name
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-transformers '(company-sort-prefer-same-case-prefix
                                                 company-sort-by-statistics))
              (setq-local company-backends '((company-capf company-files)))
              (company-mode)
              (company-statistics-mode)))
  :config
  (setq eshell-history-size 10000
        eshell-save-history-on-exit t
        eshell-directory-name (concat user-emacs-directory ".cache/eshell")))

(use-package eshell-fringe-status
  :ensure t
  :hook (eshell-mode . eshell-fringe-status-mode))

(provide 'mds-terminal)
;;; mds-terminal.el ends here
