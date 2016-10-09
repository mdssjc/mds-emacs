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
  :config
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (require 'ob-java)
  (org-babel-do-load-languages
   'org-babel-do-load-languages
   '((emacs-lisp . t)
     (java . t)
     (c . t))))

(use-package zeal-at-point
  :ensure t
  :defer t)

(use-package counsel-dash
  :ensure t
  :bind
  (("C-." . counsel-dash-at-point))
  :preface
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point"
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))
  ;; (defun counsel-dash-at-point ()
  ;;   (interactive)
  ;;   (counsel-dash (thing-at-point 'symbol)))
  :config
  (setq counsel-dash-docsets-path "~/.local/share/Zeal/Zeal/docsets"
        counsel-dash-browser-func 'eww
        counsel-dash-enable-debugging nil))

(provide 'mds-pragmatic)
;;; mds-pragmatic.el ends here
