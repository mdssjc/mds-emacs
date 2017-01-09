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

;;; Commentary:
;; Configurações para a linguagem Java.

;;; Code:
(use-package java-mode
  :mode
  ("\\.java$" . java-mode)
  :interpreter
  ("java" . java-mode)
  :init
  (add-hook 'java-mode-hook
            '(lambda () (progn
                          (semantic-mode)
                          (setq-local company-transformers '(company-sort-by-backend-importance
                                                             company-sort-prefer-same-case-prefix
                                                             company-sort-by-statistics))
                          (setq-local company-backends '((company-yasnippet
                                                          :with
                                                          company-keywords
                                                          company-abbrev
                                                          company-dabbrev-code
                                                          company-dabbrev
                                                          company-dict
                                                          company-files)))
                          (flycheck-mode)
                          (setq-local counsel-dash-docsets '("Java_SE8" "Java_EE7" "JavaFX"))
                          (define-key java-mode-map (kbd "<f6> e") 'eclim-init)
                          (define-key java-mode-map (kbd "<f6> j") 'jdee-mode)
                          (define-key java-mode-map (kbd "<f6> m") 'meghanada-mode))))
  (defun eclim-init ()
    (interactive)
    (setq-local company-backends '((company-emacs-eclim
                                    company-yasnippet
                                    :with
                                    company-keywords
                                    company-abbrev
                                    company-dabbrev-code
                                    company-dabbrev
                                    company-dict
                                    company-files)))
    (setq help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    (eclim-mode))
  (add-hook 'jdee-mode-hook
            '(lambda () (progn
                          (setq-local company-backends '((company-yasnippet
                                                          :with
                                                          company-keywords
                                                          company-abbrev
                                                          company-dabbrev-code
                                                          company-dabbrev
                                                          company-dict
                                                          company-files))))))
  (add-hook 'meghanada-mode-hook
            '(lambda () (progn
                          (setq-local company-backends '((company-meghanada
                                                          company-yasnippet
                                                          :with
                                                          company-keywords
                                                          company-abbrev
                                                          company-dabbrev-code
                                                          company-dabbrev
                                                          company-dict
                                                          company-files)))))))

(use-package java-snippets
  :ensure t
  :defer t)

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :config
  (setq meghanada-server-install-dir (concat user-emacs-directory ".cache/meghanada/")))

(use-package jdee
  :ensure t
  :commands jdee-mode
  :config
  (setq jdee-server-dir (concat user-emacs-directory ".cache/jdee/")))

(use-package eclim
  :ensure t
  :commands global-eclim-mode eclim-mode
  :bind
  (:map eclim-mode-map
        ("C-c C-d s" . start-eclimd)
        ("C-c C-d k" . stop-eclimd))
  :config
  (use-package company-emacs-eclim
    :ensure t
    :commands company-emacs-eclim)
  (use-package eclimd
    :commands start-eclimd))

(provide 'mds-java-pl)
;;; mds-java-pl.el ends here
