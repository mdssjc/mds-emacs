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
(defface rec-face
  '((t (:background "red" :foreground "white" :weight bold)))
  "Flag macro recording in mode-line"
  :group 'mds-aesthetic-modeline-faces)

;;; Code:
(setq-default mode-line-format
              '(""
                mode-line-front-space
                (:eval (if (eq defining-kbd-macro t)
                           (concat (propertize "[M]" 'face 'rec-face) " ")))
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
                               (concat
                                " Iedit: "
                                (propertize (format "%d" (iedit-counter)) 'face 'font-lock-warning-face)))))
                (org-agenda-mode (:eval (format "%s" org-agenda-filter)))
                " "
                ;;mode-line-modes
                (:eval (propertize (if overwrite-mode "Ovr" "")
                                   'face 'font-lock-warning-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))
                (:eval (if overwrite-mode " " ""))
                mode-line-misc-info
                "::"
                mode-line-position
                mode-line-end-spaces))

(display-time-mode)

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
