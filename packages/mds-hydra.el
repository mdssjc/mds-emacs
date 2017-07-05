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

;; Embrace
(defhydra hydra-embrace (:color blue :hint nil)
  "
Add (_a_), change (_c_) or delete (_d_) a pair.
  "
  ("<ESC>" nil "quit")
  ("a" embrace-add)
  ("c" embrace-change)
  ("d" embrace-delete))
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

;; Dumb Jump
(defhydra hydra-dumb-jump (:color blue :hint nil)
  "Dumb Jump"
  ("<ESC>" nil nil)
  ("<SPC>" dumb-jump-go-prompt                   "Prompt")
  ("g" dumb-jump-go                              "Go")
  ("b" dumb-jump-back                            "Back")
  ("q" dumb-jump-quick-look                      "Quick Look")
  ("o" dumb-jump-go-other-window                 "Other Window")
  ("x" dumb-jump-go-prefer-external              "External")
  ("z" dumb-jump-go-prefer-external-other-window "External Window"))
;; ---

(provide 'mds-hydra)
;;; mds-hydra.el ends here
