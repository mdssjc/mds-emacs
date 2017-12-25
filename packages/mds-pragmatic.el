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
(defun gtd ()
  "Internal function."
  (interactive "")
  (find-file "~/Documents/GTD/dashboard.org"))

(use-package org
  :ensure t
  :mode
  (("\\.org\\'" . org-mode))
  :init
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (add-hook
      'org-mode-hook
      (lambda ()
        (setq-local company-backends '((company-capf
                                        company-abbrev
                                        company-dabbrev
                                        company-yasnippet
                                        company-ispell)))
        (company-mode)
        (embrace-org-mode-hook)
        (worf-mode)
        (org-bullets-mode t)
        (org-sticky-header-mode)
        (org-table-sticky-header-mode)
        (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
        (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
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
              org-default-notes-file (concat user-emacs-directory "org/notes.org")
              org-agenda-files (list "~/Documents/GTD/someday-maybe.org"
                                     "~/Documents/GTD/calendar.org")
              org-tag-alist '(("work" . ?w))
              org-ellipsis "⤵"
              org-pretty-entities t
              org-startup-align-all-tables t
              org-src-fontify-natively nil
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              org-ditaa-jar-path    (expand-file-name "~/java/ditaa.jar")
              org-plantuml-jar-path (expand-file-name "~/java/plantuml.jar")
              ;; Agenda
              org-agenda-custom-commands '(("W" "Weekly Review"
                                            ((agenda "" ((org-agenda-ndays 7)))
                                             (stuck "")
                                             (todo "DOING"))))
              ;; Refile
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-refile-allow-creating-parent-nodes 'confirm
              org-refile-targets '(("~/Documents/GTD/trash.org"         :maxlevel . 1)
                                   ("~/Documents/GTD/someday-maybe.org" :maxlevel . 1)
                                   ("~/Documents/GTD/reference.org"     :maxlevel . 1)
                                   ("~/Documents/GTD/projects.org"      :maxlevel . 3)
                                   ("~/Documents/GTD/next-action.org"   :maxlevel . 2)
                                   ("~/Documents/GTD/waiting-for.org"   :maxlevel . 1))
              ;; Templates - Capture
              org-capture-templates '(("n" "Notes" entry
                                       (file+headline (concat user-emacs-directory "org/notes.org") "Notes")
                                       "* %i%?")
                                      ("t" "Inbox" entry
                                       (file+headline "~/Documents/GTD/inbox.org" "Things")
                                       "* %i%?\nCreate in %U")))
        (require 'ob-java)
        (require 'ob-C)
        (require 'ob-ditaa)
        (require 'ob-plantuml)
        (require 'org-crypt)
        (require 'ob-translate)
        (org-babel-do-load-languages 'org-babel-load-languages
                                     '((emacs-lisp . t)
                                       (java       . t)
                                       (C          . t)
                                       (dot        . t)
                                       (ditaa      . t)
                                       (plantuml   . t)))
        (add-to-list 'org-src-lang-modes
                     '("plantuml" . plantuml))
        ;; Encrypt all entries before saving
        (org-crypt-use-before-save-magic)
        (setq org-tags-exclude-from-inheritance (quote ("crypt")))
        ;; GPG key to use for encryption
        (when (file-exists-p (concat user-emacs-directory "secrets/secrets.el"))
          (setq org-crypt-key user-password))))))
  :config
  (setq org-agenda-files (seq-filter (lambda (filename) (file-exists-p filename))
                                     '("~/Documents/GTD/someday-maybe.org"
                                       "~/Documents/GTD/calendar.org"))))

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

(use-package ob-translate
  :ensure t
  :defer t)

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
