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
(defvar langtool-path "/home/mdssjc/Documents/Git/languagetool/languagetool-standalone/target/LanguageTool-3.6-SNAPSHOT/LanguageTool-3.6-SNAPSHOT/")

(use-package flycheck
  :ensure t
  :diminish flycheck-mode " ⓢ"
  :pin melpa
  :bind
  (("<f5> s" . flycheck-mode))
  :config
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))
  (setq flycheck-check-syntax-automatically '(mode-enable save)))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode " Ⓢ"
  :bind
  (("<f5> S" . flyspell-mode))
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flycheck-mode)
  (use-package flyspell-correct-popup
    :ensure t
    :config
    (setq flyspell-correct-interface 'flyspell-correct-popup))
  (use-package flyspell-correct-ivy
    :ensure t
    :bind (:map flyspell-mode-map
                ("C-c $" . flyspell-correct-previous-word-generic))))

(use-package langtool
  :ensure t
  :bind
  (("<f8> l c" . langtool-check)
   ("<f8> l d" . langtool-check-done)
   ("<f8> l b" . langtool-correct-buffer)
   ("<f8> l s" . langtool-switch-default-language)
   ("<f8> l ." . langtool-show-message-at-point))
  :config
  (setq langtool-language-tool-jar (concat langtool-path "languagetool-commandline.jar")
        langtool-default-language "pt-BR"
        langtool-mother-tongue "pt-BR"
        langtool-autoshow-message-function (lambda (overlays)
                                             (when (require 'popup nil t)
                                               (unless (or popup-instances
                                                           (memq last-command '(keyboard-quit)))
                                                 (let ((msg (langtool-details-error-message overlays)))
                                                   (popup-tip msg)))))))

(provide 'mds-semantic)
;;; mds-semantic.el ends here
