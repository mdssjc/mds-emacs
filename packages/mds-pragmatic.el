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
   ("<f9> b" . org-iswitchb)
   ("<f9> p" . org-pomodoro)
   ("<f9> T" . tomatinho))
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
               (flyspell-mode -1)
               (org-bullets-mode t)
               (worf-mode)
               (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
  :config
  (require 'ob-java)
  (require 'ob-C)
  (require 'ob-ditaa)
  (require 'ob-plantuml)
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
        org-agenda-files (list "~/"
                               "~/Documents"
                               (concat user-emacs-directory "org"))
        org-default-notes-file (concat user-emacs-directory "org/notes.org")
        org-ellipsis "⤵"
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-align-all-tables t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-ditaa-jar-path    (expand-file-name "~/java/ditaa.jar")
        org-plantuml-jar-path (expand-file-name "~/java/plantuml.jar"))
  (org-babel-do-load-languages 'org-babel-do-load-languages '((emacs-lisp . t)
                                                              (java       . t)
                                                              (c          . t)
                                                              (ditaa      . t)
                                                              (plantuml   . t))))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode)

(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro)

(use-package tomatinho
  :ensure t
  :commands tomatinho)

(use-package worf
  :ensure t
  :commands worf-mode)

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
