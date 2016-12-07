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
    ("e" er/mark-email              "email")
    ("0" nil                        "quit"))
;; ---

;; Selected
(defhydra hydra-selected (:color blue :hint nil)
    "
 Selected
 Case: _U_p | _D_own                          count _W_ords                           ^^^^^^+-^^----------+
 Camel-case: _l_ower | _u_pper                apply _m_acro                           ^^^^^^| ^^ Org Mode |
 Transformation: _w_ords | _s_nake | _d_ashed | _c_apitalized | _t_itleized  | _i_nitials   | _e_mphasize |
^^^^^^^^^^^^                                                                                +-^^----------+
    "
    ("<ESC>" nil "quit")
    ("q" selected-off)
    ("U" upcase-region)
    ("D" downcase-region)
    ("W" count-words-region)
    ("m" apply-macro-to-region-lines)
    ("w" mds/split-words)
    ("l" mds/lower-camel-case)
    ("u" mds/upper-camel-case)
    ("s" mds/snake-case)
    ("d" mds/dashed-words)
    ("c" mds/capitalized-words)
    ("t" mds/titleized-words)
    ("i" mds/word-initials)
    ("e" org-emphasize))
;; ---

(provide 'mds-hydra)
;;; mds-hydra.el ends here
