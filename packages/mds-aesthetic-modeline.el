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
(defface rec-face
  '((t (:background "red" :foreground "white" :weight bold)))
  "Flag macro recording in mode-line"
  :group 'mds-aesthetic-modeline-faces)

(setq display-time-format "%H:%M"
      display-time-string-forms
      '((propertize
         (format-time-string (or display-time-format
                                 (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                             now)
         'help-echo (concat (format-time-string "%c; ")
                            (emacs-uptime "Uptime:%hh")))
        load
        (if mail
            (concat
             " "
             (propertize
              display-time-mail-string
              'display `(when (and display-time-use-mail-icon
                                   (display-graphic-p))
                          ,@display-time-mail-icon
                          ,@(if (and display-time-mail-face
                                     (memq (plist-get (cdr display-time-mail-icon)
                                                      :type)
                                           '(pbm xbm)))
                                (let ((bg (face-attribute display-time-mail-face
                                                          :background)))
                                  (if (stringp bg)
                                      (list :background bg)))))
              'face display-time-mail-face
              'help-echo "You have new mail; mouse-2: Read mail"
              'mouse-face 'mode-line-highlight
              'local-map (make-mode-line-mouse-map 'mouse-2
                                                   read-mail-command)))
          "")))

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
                (:propertize mode-line-buffer-identification
                             help-echo (concat "Buffer name"
                                               "\nmouse-1: Previous buffer"
                                               "\nmouse-3: Next buffer"
                                               "\n" (buffer-file-name)))
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
                "[" (:eval (format "%d" total-lines)) "]"
                mode-line-end-spaces))

(display-time-mode)

(use-package total-lines
  :ensure t
  :hook (after-init . global-total-lines-mode))

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
