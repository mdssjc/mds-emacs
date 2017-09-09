;;; mds-core-funcs.el --- Funções (Functions)
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
;; Funções personalizadas.

;;; Code:
;; Quebra de Linha (Break Line)
(defun mds/insert-lines-above (times)
  "Insert TIMES break lines above the cursor."
  (interactive "p")
  (beginning-of-line)
  (newline times)
  (previous-line times)
  (indent-according-to-mode))

(defun mds/insert-lines-below (times)
  "Insert TIMES break lines below the cursor."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (newline times))
  (next-line times)
  (indent-according-to-mode))

(defun mds/insert-lines-between (times)
  "Insert TIMES break lines between the cursor."
  (interactive "p")
  (let ((position (current-column)))
    (mds/insert-lines-above times)
    (next-line times)
    (mds/insert-lines-below times)
    (previous-line times)
    (forward-char position)))
;; ---

;; Formatação de texto
(defun mds//text-case (func begin end)
  (let* ((text-selected (s-trim (buffer-substring-no-properties begin end)))
         (result (funcall func text-selected)))
    (message "Changed of %s to %s." text-selected result)
    (delete-region begin end)
    (insert result)))

(defun mds//list-to-string (s)
  (format "%s" (mapconcat 'identity (s-split-words s) " ")))

(defun mds/split-words (begin end)
  (interactive "r")
  (mds//text-case #'mds//list-to-string begin end))

(defun mds/lower-camel-case (begin end)
  (interactive "r")
  (mds//text-case #'s-lower-camel-case begin end))

(defun mds/upper-camel-case (begin end)
  (interactive "r")
  (mds//text-case #'s-upper-camel-case begin end))

(defun mds/snake-case (begin end)
  (interactive "r")
  (mds//text-case #'s-snake-case begin end))

(defun mds/dashed-words (begin end)
  (interactive "r")
  (mds//text-case #'s-dashed-words begin end))

(defun mds/capitalized-words (begin end)
  (interactive "r")
  (mds//text-case #'s-capitalized-words begin end))

(defun mds/titleized-words (begin end)
  (interactive "r")
  (mds//text-case #'s-titleized-words begin end))

(defun mds/word-initials (begin end)
  (interactive "r")
  (mds//text-case #'s-word-initials begin end))
;; ---

;; Maio
(defun maio/electric-semicolon ()
  "Insert a semicolon in expression."
  (interactive)
  (end-of-line)
  (when (not (looking-back ";" 0))
    (insert ";")))
;; ---

;; Xah
(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-select-line ()
  "Select current line.
If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (xah-select-current-line)))
;; ---

(provide 'mds-core-funcs)
;;; mds-core-funcs.el ends here
