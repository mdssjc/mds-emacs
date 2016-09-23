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
;; Analisa o texto e/ou código.

;;; Code:
(use-package flycheck
  :ensure t
  :diminish flycheck-mode " ⓢ"
  :pin melpa
  :bind (("<f5> s" . flycheck-mode))
  :config
  (use-package flycheck-package :ensure t)
  (use-package flycheck-pos-tip
    :ensure t
    :init (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))
  (setq flycheck-check-syntax-automatically '(mode-enable save))
  (eval-after-load 'flycheck '(flycheck-package-setup)))

;; Em configuração
(use-package flyspell
  :ensure t
  :diminish flyspell-mode " Ⓢ"
  :bind (("<f5> S" . flyspell-mode))
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flycheck-mode)
  (use-package flyspell-correct-popup
    :ensure t
    :commands (flyspell-correct-word-generic
               flyspell-correct-previous-word-generic)
    :config
    (setq flyspell-correct-interface 'flyspell-correct-popup))
  (use-package flyspell-correct-ivy
    :ensure t
    :bind (:map flyspell-mode-map
                ("C-c $" . flyspell-correct-word-generic))))

(provide 'mds-semantic)
;;; mds-semantic.el ends here
