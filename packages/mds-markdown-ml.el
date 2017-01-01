;;; mds-markdown-ml.el --- Linguagem de Marcação Markdown (Markdown Markup Language)
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
;; Markdown: arquivos .md.

;;; Code:
(use-package markdown-mode
  :ensure t
  :commands markdown-mode gfm-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq tab-width 4))

(provide 'mds-markdown-ml)
;;; mds-markdown-ml.el ends here
