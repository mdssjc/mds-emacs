;;; mds-pragmatic.el --- Pragmático (Pragmatic)
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
;; Organização do trabalho.

;;; Code:
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("<f9> l" . org-store-link)
   ("<f9> a" . org-agenda)
   ("<f9> c" . org-capture)
   ("<f9> b" . org-iswitchb))
  :init
  (add-hook 'org-mode-hook
            '(lambda () (progn)
               (setq-local company-minimum-prefix-length 3)
               (setq-local company-transformers '(company-sort-by-occurrence
                                                  company-sort-prefer-same-case-prefix
                                                  company-sort-by-statistics))
               (setq-local company-backends '((company-capf
                                               company-abbrev
                                               company-dabbrev
                                               company-yasnippet
                                               :with
                                               company-ispell)))
               (company-mode)
               (flyspell-mode)
               (org-bullets-mode 1)))
  :config
  (require 'ob-java)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW"
                                      "|" "DONE" "ARCHIVED" "CANCELED"))
        org-todo-keyword-faces '(("TODO"     . org-warning)
                                 ("DOING"    . "yellow")
                                 ("BLOCKED"  . "red")
                                 ("REVIEW"   . "orange")
                                 ("DONE"     . "green")
                                 ("ARCHIVED" . "blue")
                                 ("CANCELED" . "red1"))
        org-directory (concat user-emacs-directory "org")
        org-default-notes-file (concat user-emacs-directory "org/notes.org"))
  (org-babel-do-load-languages 'org-babel-do-load-languages '((emacs-lisp . t)
                                                              (java . t)
                                                              (c . t))))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode)

(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro)

(use-package tomatinho
  :ensure t
  :commands tomatinho)

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
