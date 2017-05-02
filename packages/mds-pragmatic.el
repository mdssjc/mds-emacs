;;; mds-pragmatic.el --- Pragmático (Pragmatic) -*- lexical-binding: t -*-
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
;; Organização do trabalho com Org.

;;; Code:
(use-package org
  :ensure t
  :mode
  (("\\.org\\'" . org-mode))
  :init
  (add-hook 'org-mode-hook
            '(lambda ()
               ;(setq-local company-minimum-prefix-length 1)
               ;(setq-local company-transformers '(company-sort-by-occurrence))
               (setq-local company-backends '((company-capf
                                               company-abbrev
                                               company-dabbrev
                                               company-yasnippet
                                               :with
                                               company-ispell)))
               (company-mode)
               (flyspell-mode -1)
               (embrace-org-mode-hook)
               (worf-mode)
               (org-bullets-mode t)
               (org-sticky-header-mode)
               (org-table-sticky-header-mode)
               (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
  :config
  (require 'ob-java)
  (require 'ob-C)
  (require 'ob-ditaa)
  (require 'ob-plantuml)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "BLOCKED(b)" "REVIEW(r)"
                                      "|" "DONE(e)" "ARCHIVED(a)" "CANCELED(c)"))
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
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (java       . t)
                                                           (C          . t)
                                                           (ditaa      . t)
                                                           (plantuml   . t)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

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

(use-package org-sticky-header
  :ensure t
  :commands org-sticky-header-mode
  :config
  (setq org-sticky-header-full-path 'full))

(use-package org-table-sticky-header
  :ensure t
  :diminish org-table-sticky-header-mode
  :commands org-table-sticky-header-mode)

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
