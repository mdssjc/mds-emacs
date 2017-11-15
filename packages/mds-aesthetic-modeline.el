;;; mds-aesthetic-modeline.el --- Estético - Modeline (Modeline - Aesthetic) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Complemento do Estético com as configurações do Modeline.

;;; Code:
(setq-default mode-line-format
              '(""
                mode-line-front-space
                mode-line-mule-info
                ": "
                mode-name " "
                (anzu-mode (:eval (anzu--update-mode-line)))
                mode-line-modified
                mode-line-directory
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                vc-mode
                flycheck-mode-line
                (iedit-mode (:eval
                             (if (= (iedit-counter) 0)
                                 ""
                               (format (concat " Iedit: " (propertize "%d" 'face 'font-lock-warning-face)) (iedit-counter)))))
                (org-agenda-mode (:eval (format "%s" org-agenda-filter)))
                " "
                ;;mode-line-modes
                mode-line-misc-info
                "::"
                mode-line-position
                ;;(:eval 'display-time-string)
                mode-line-end-spaces))

(display-time-mode)

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
