;;; mds-java-pl.el --- Linguagem de Programação Java (Java Programming Language)
;;
;; Copyright (C) 2016-2016 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense
;; FIXME -> o pacote está em testes
(use-package meghanada
  :ensure t
  :init (add-hook 'java-mode-hook (lambda () (meghanada-mode t))))

(provide 'mds-java-pl)
;;; mds-java-pl ends here
