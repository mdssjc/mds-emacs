;;; mds-hydra.el --- Hydra (Hydra) -*- lexical-binding: t -*-
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
;; Definições do Hydra.

;;; Code:
;; Expand-Region
(defhydra hydra-expand-region (:columns 4 :color blue)
  "Mark"
  ("<ESC>" nil "quit")
  ("w" er/mark-word               "word")
  ("s" er/mark-symbol             "symbol")
  ("S" er/mark-symbol-with-prefix "symbol-with-prefix")
  ("n" er/mark-next-accessor      "next-accessor")
  ("m" er/mark-method-call        "method-call")
  ("Q" er/mark-inside-quotes      "inside-quotes")
  ("\"" er/mark-inside-quotes     "inside-quotes")
  ("q" er/mark-outside-quotes     "outside-quotes")
  ("P" er/mark-inside-pairs       "inside-pairs")
  ("\(" er/mark-inside-pairs      "inside-pairs")
  ("p" er/mark-outside-pairs      "outside-pairs")
  ("c" er/mark-comment            "comment")
  ("u" er/mark-url                "url")
  ("e" er/mark-email              "email")
  ("d" er/mark-defun              "defun"))
;; ---

;; YASnippets
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
                  ^^^^YASnippets^^^^
--^------^---^-----------^---^--------^--^-------^--
  ^Modes:^   ^Load/Visit:^   ^Actions:^  ^Others:^
  _g_lobal   _d_irectory     _i_nsert    _c_reate
  _m_inor    _f_ile          _t_ryout    e_x_pand
  _e_xtra    _l_ist          _n_ew       _o_pen
  ^ ^        _a_ll
  "
  ("<ESC>" nil nil)
  ("g" yas-global-mode)
  ("m" yas-minor-mode)
  ("e" yas-activate-extra-mode)
  ("d" yas-load-directory)
  ("f" yas-visit-snippet-file)
  ("l" yas-describe-tables)
  ("a" yas-reload-all)
  ("i" yas-insert-snippet)
  ("t" yas-tryout-snippet)
  ("n" yas-new-snippet)
  ("c" aya-create)
  ("x" aya-expand)
  ("o" aya-open-line))
;; ---

(provide 'mds-hydra)
;;; mds-hydra.el ends here
