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
(defun mds/insert-lines-between (times)
  "Insert a break line between the cursor."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (newline times)
    (newline times)
    (beginning-of-line))
  (dotimes (i times) (mds/move-down)))

(defun mds/move-up ()
  "Moving the line/region to up."
  (interactive)
  (if (region-active-p)
      (let* ((line-text (delete-and-extract-region (region-beginning) (region-end)))
             (lines (* -1 (- (length (split-string line-text "\n")) 1))))
        (message "Moving region...")
        (set-mark-command 0)
        (forward-line -1)
        (insert line-text)
        (set-mark-command nil)
        (forward-line lines)
        (setq deactivate-mark nil))
    (progn
      (let ((column (current-column)))
        (message "Moving line...")
        (transpose-lines 1)
        (forward-line -2)
        (forward-char column)))))

(defun mds/move-down ()
  "Moving the line/region to down."
  (interactive)
  (if (region-active-p)
      (let* ((line-text (delete-and-extract-region (region-beginning) (region-end)))
             (lines (* -1 (- (length (split-string line-text "\n")) 1))))
        (message "Moving region...")
        (set-mark-command 0)
        (forward-line 1)
        (insert line-text)
        (forward-line lines)
        (set-mark-command nil)
        (forward-line (* -1 lines))
        (backward-char 1)
        (setq deactivate-mark nil))
    (progn
      (let ((column (current-column)))
        (message "Moving line...")
        (forward-line 1)
        (transpose-lines 1)
        (forward-line -1)
        (forward-char column)))))

(defun mds/duplicate-up (arg)
  (interactive "p")
  (beginning-of-line)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (yank)
  (forward-line -1))

(defun mds/duplicate-down (arg)
  (interactive "p")
  (beginning-of-line)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (yank))

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

(provide 'mds-core-funcs)
;;; mds-core-funcs.el ends here
