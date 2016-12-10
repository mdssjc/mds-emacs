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

;; Selected
(defhydra hydra-selected (:columns 5 :color blue)
  "Selected"
  ("<ESC>" nil "quit")
  ("$" 'flyspell-region             "Spell")
  ("q" 'selected-off                "Off")
  ("k" 'capitalize-region           "Capitalize")
  ("u" 'upcase-region               "Upcase")
  ("l" 'downcase-region             "Downcase")
  ("w" 'count-words-region          "Count Words")
  ("m" 'apply-macro-to-region-lines "Apply Macro")
  ("c" 'kill-ring-save              "Copy")
  ("x" 'kill-region                 "Cut")
  ("p" 'yank                        "Paste")
  ("C-s s" 'sort-lines              "Sort Lines")
  ("C-s r" 'reverse-region          "Reverse Lines")
  ("C-x w" 'mds/split-words         "Split")
  ("C-x l" 'mds/lower-camel-case    "Lower")
  ("C-x u" 'mds/upper-camel-case    "Upper")
  ("C-x s" 'mds/snake-case          "Snake")
  ("C-x d" 'mds/dashed-words        "Dashed")
  ("C-x c" 'mds/capitalized-words   "Capitalized")
  ("C-x t" 'mds/titleized-words     "Titleized")
  ("C-x i" 'mds/word-initials       "Initials"))
;; ---

(provide 'mds-hydra)
;;; mds-hydra.el ends here
