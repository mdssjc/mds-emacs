;;; mds-plantuml-dsl.el --- Linguagem de Domínio Específico (Domain Specific Language)
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
;; Configurações para o PlantUML.

;;; Code:
(use-package plantuml-mode
  :ensure t
  :mode
  ("\\.plantuml\\'" . plantuml-mode)
  :init
  (add-hook 'plantuml-mode-hook
            '(lambda ()
               (flycheck-mode)
               (eval-after-load 'flycheck '(flycheck-plantuml-setup))))
  :config
  (setq plantuml-jar-path "~/java/plantuml.jar"))

(use-package flycheck-plantuml
  :commands flycheck-plantuml-setup)

(provide 'mds-plantuml-dsl)
;;; mds-plantuml-dsl.el ends here
