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
  :config
  (setq org-todo-keywords '((sequence "TODO"
                                      "DOING"
                                      "BLOCKED"
                                      "REVIEW"
                                      "|" "DONE" "ARCHIVED" "CANCELED"))
        org-todo-keyword-faces '(("TODO"     . org-warning)
                                 ("DOING"    . "yellow")
                                 ("BLOCKED"  . "red")
                                 ("REVIEW"   . "orange")
                                 ("DONE"     . "green")
                                 ("ARCHIVED" . "blue")
                                 ("CANCELED" . "red1")))
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (require 'ob-java)
  (org-babel-do-load-languages 'org-babel-do-load-languages
                               '((emacs-lisp . t)
                                 (java . t)
                                 (c . t))))

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
