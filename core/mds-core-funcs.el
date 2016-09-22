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

(provide 'mds-core-funcs)
;;; mds-core-funcs.el ends here
