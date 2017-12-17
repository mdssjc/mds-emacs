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

(setq column-number-mode   t
      size-indication-mode t
      display-time-format "%H:%M"
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
              'local-map (make-mode-line-mouse-map 'mouse-2 read-mail-command)))
          "")))

(defun custom-modeline-buffer-identification()
  '(:propertize mode-line-buffer-identification
                help-echo (concat "Buffer name"
                                  "\nmouse-1: Previous buffer"
                                  "\nmouse-2: Switch to another buffer"
                                  "\nmouse-3: Next buffer"
                                  "\npath: " (buffer-file-name))
                mouse-face 'mode-line-highlight
                local-map (keymap
                           (mode-line . (keymap (mouse-1 . previous-buffer)
                                                (mouse-2 . ivy-switch-buffer)
                                                (mouse-3 . next-buffer))))))

(defun custom-modeline-region-info()
  (when mark-active
    (let ((chars (count-matches "."  (region-end) (region-beginning)))
          (words (count-words-region (region-end) (region-beginning)))
          (lines (count-lines (region-beginning) (region-end))))
      (concat
       " "
       (propertize (all-the-icons-octicon "pencil")
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise 0.1))
       (propertize (format " (%s,%s,%s)" chars words lines)
                   'face `(:height 0.9))))))

(defun custom-modeline-parinfer()
  '((parinfer-mode " ")
    (parinfer-mode
     (:propertize
      (:eval (parinfer--lighter))
      help-echo (concat "Parinfer"
                        "\nmouse-1: Toggle mode")
      mouse-face 'mode-line-highlight
      local-map (keymap
                 (mode-line . (keymap (mouse-1 . parinfer-toggle-mode))))))))

(defun custom-modeline-flycheck()
  '(flycheck-mode
    (:propertize flycheck-mode-line
                 help-echo (concat "Flycheck"
                                   "\nmouse-1: Show all errors"
                                   "\nmouse-3: Check buffer")
                 mouse-face 'mode-line-highlight
                 local-map (keymap
                            (mode-line . (keymap (mouse-1 . flycheck-list-errors)
                                                 (mouse-3 . flycheck-buffer)))))))

(defun custom-modeline-overwrite()
  '((overwrite-mode " ")
    (overwrite-mode
     (:propertize (if overwrite-mode "Ovr" "")
                  face 'font-lock-warning-face
                  help-echo (concat "Buffer is in "
                                    (if overwrite-mode "overwrite" "insert")
                                    " mode")))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:eval (if (eq defining-kbd-macro t)
                           (concat (propertize "[M]" 'face 'rec-face) " ")))
                mode-line-mule-info
                ": "
                (:propertize mode-name
                             help-echo (format "%s" major-mode))
                " "
                (anzu-mode (:eval (anzu--update-mode-line)))
                mode-line-modified
                ;; mode-line-frame-identification
                (:eval (custom-modeline-buffer-identification))
                mode-line-process
                vc-mode
                (:eval (custom-modeline-region-info))
                (:eval (custom-modeline-parinfer))
                (:eval (custom-modeline-flycheck))
                (flyspell-mode (:eval (format " %s:%s"
                                              flyspell-mode-line-string
                                              (split-string ispell-current-dictionary "_"))))
                (iedit-mode (:eval
                             (if (= (iedit-counter) 0)
                                 ""
                               (concat
                                " Iedit: "
                                (propertize (format "%d" (iedit-counter)) 'face 'font-lock-warning-face)))))
                (org-agenda-mode (:eval (format "%s" org-agenda-filter)))
                ;; mode-line-modes
                ;; minor-mode-alist
                (:eval (custom-modeline-overwrite))
                " "
                mode-line-misc-info
                celestial-mode-line-string
                " ("
                (company-mode company-lighter)
                ") "
                "::"
                mode-line-position
                "[" (:eval (format "%d" total-lines)) "]"
                mode-line-end-spaces))

(display-time-mode)

(use-package celestial-mode-line
  :ensure t
  :config
  (setq calendar-longitude     longitude
        calendar-latitude      latitude
        calendar-location-name user-city)
  (celestial-mode-line-start-timer))

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
