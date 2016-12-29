;;; mds-hydra.el --- Hydra (Hydra)
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
;; Definições do Hydra.

;;; Code:
;; Ace-Window
(defhydra hydra-window-size (:color red)
  "Windows size"
  ("h" shrink-window-horizontally  "shrink horizontal")
  ("j" shrink-window               "shrink vertical")
  ("k" enlarge-window              "enlarge vertical")
  ("l" enlarge-window-horizontally "enlarge horizontal"))

(defhydra hydra-window-frame (:color red)
  "Frame"
  ("f" make-frame   "new frame")
  ("x" delete-frame "delete frame"))

(defhydra hydra-window-scroll (:color red)
  "Scroll other window"
  ("n" scroll-other-window      "scroll")
  ("p" scroll-other-window-down "scroll down"))
;; ---

;; Expand-Region
(defhydra hydra-expand-region (:columns 4 :color blue)
  "Mark"
  ("<ESC>" nil "quit")
  ("w" er/mark-word               "word")
  ("s" er/mark-symbol             "symbol")
  ("d" er/mark-defun              "defun")
  ("P" er/mark-inside-pairs       "inside-pairs")
  ("p" er/mark-outside-pairs      "outside-pairs")
  ("Q" er/mark-inside-quotes      "inside-quotes")
  ("q" er/mark-outside-quotes     "outside-quotes")
  ("." er/mark-sentence           "sentence")
  ("h" er/mark-paragraph          "paragraph")
  ("S" er/mark-symbol-with-prefix "symbol-with-prefix")
  ("n" er/mark-next-accessor      "next-accessor")
  ("m" er/mark-method-call        "method-call")
  ("c" er/mark-comment            "comment")
  ("u" er/mark-url                "url")
  ("e" er/mark-email              "email"))
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
  ("q" nil nil)
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
