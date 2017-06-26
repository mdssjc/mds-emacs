;;; mds-markdown-ml.el --- Linguagem de Marcação Markdown (Markdown Markup Language) -*- lexical-binding: t -*-
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
;; Configurações para a linguagem Markdown.

;;; Code:
(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (setq-local company-backends '((company-capf
                                               company-abbrev
                                               company-dabbrev
                                               company-ispell)))))
  :config
  (setq tab-width 2
        markdown-italic-underscore t))

(provide 'mds-markdown-ml)
;;; mds-markdown-ml.el ends here
