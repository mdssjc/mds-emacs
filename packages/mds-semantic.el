;;; mds-semantic.el --- Semântico (Semantic)
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
;; Analisa o código.

;;; Code:
(use-package flycheck
  :ensure t
  :diminish flycheck-mode " ⓢ"
  :pin melpa
  :config
  (use-package flycheck-package :ensure t)
  (use-package flycheck-pos-tip
    :ensure t
    :init (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))
  (setq flycheck-check-syntax-automatically '(mode-enable save))
  (eval-after-load 'flycheck '(flycheck-package-setup)))

(provide 'mds-pragmatic)
;;; mds-semantic.el ends here
