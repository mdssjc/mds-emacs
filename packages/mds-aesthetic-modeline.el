;;; mds-aesthetic-modeline.el --- Estético - Modeline (Modeline - Aesthetic) -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2018 Marcelo dos Santos
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
(setq column-number-mode   t
      size-indication-mode t
      iedit-mode-line `(" Iedit("
                        (:eval (format ,(propertize "%d"
                                                    'face 'font-lock-warning-face)
                                       (iedit-counter)))
                        ")")
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

(defun custom-modeline-buffer-identification ()
  '(:propertize mode-line-buffer-identification
                help-echo (concat "Buffer name"
                                  "\nmouse-1: Previous buffer"
                                  "\nmouse-2: Switch to another buffer"
                                  "\nmouse-3: Next buffer"
                                  "\npath: " (buffer-file-name))
                face '((t (:background "grey20" :foreground "white")))
                mouse-face mode-line-highlight
                local-map (keymap
                           (mode-line . (keymap (mouse-1 . previous-buffer)
                                                (mouse-2 . ivy-switch-buffer)
                                                (mouse-3 . next-buffer))))))

(defun custom-modeline-position-info ()
  `(" ("
    ,(propertize "%l"
                 'face 'font-lock-type-face
                 'help-echo (format "%d lines" (count-lines (point-min)
                                                            (point-max))))
    ","
    ,(propertize "%C"
                 'face 'font-lock-type-face
                 'help-echo (format "%d lines" (count-lines (point-min)
                                                            (point-max))))
    ")"))

(defun custom-modeline-size-info ()
  `(" ["
    (:propertize (-3 "%p") face font-lock-constant-face)
    "/"
    ,(propertize "%I"      'face 'font-lock-constant-face)
    "]"))

(defun custom-modeline-overwrite ()
  `(,(concat " "
             (propertize "Ovr"
                         'face      'font-lock-warning-face
                         'help-echo (concat "Buffer is in "
                                            (if overwrite-mode "overwrite" "insert")
                                            " mode")))))

(defun custom-modeline-region-info ()
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

(defun custom-modeline-parinfer ()
  '(" "
    (:propertize
     (:eval (parinfer--lighter))
     help-echo (concat "Parinfer"
                       "\nmouse-1: Toggle mode")
     mouse-face mode-line-highlight
     local-map (keymap
                (mode-line . (keymap (mouse-1 . parinfer-toggle-mode)))))))

(defun custom-modeline-flycheck ()
  `(,(concat " "
             (propertize (string-trim (flycheck-mode-line-status-text))
                         'help-echo  (concat "Flycheck"
                                             "\nmouse-1: Show all errors"
                                             "\nmouse-3: Check buffer")
                         'mouse-face 'mode-line-highlight
                         'local-map  '(keymap
                                       (mode-line . (keymap (mouse-1 . flycheck-list-errors)
                                                            (mouse-3 . flycheck-buffer))))))))

(defun custom-modeline-flyspell ()
  `(,(format " %s:%s"
             flyspell-mode-line-string
             (split-string ispell-current-dictionary "_"))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:eval (if (eq defining-kbd-macro t)
                           (concat (propertize "[M]" 'face '((t (:background "red" :foreground "white" :weight bold)))) " ")))
                mode-line-mule-info
                mode-line-modified

                ;; mode-line-frame-identification
                (:eval (custom-modeline-buffer-identification))
                (:propertize "%n" face font-lock-warning-face)
                (:eval (custom-modeline-position-info))
                (:eval (custom-modeline-size-info))

                " "
                (:propertize mode-name
                             help-echo (format "%s" major-mode))

                mode-line-process
                (overwrite-mode   (:eval (custom-modeline-overwrite)))
                (vc-mode vc-mode)
                (:eval (custom-modeline-region-info))
                (parinfer-mode    (:eval (custom-modeline-parinfer)))
                (iedit-mode       iedit-mode-line)
                (multiple-cursors-mode mc/mode-line)
                (flycheck-mode    (:eval (custom-modeline-flycheck)))
                (flyspell-mode    (:eval (custom-modeline-flyspell)))
                (indent-info-mode (:eval (indent-info-mode-line)))
                (org-agenda-mode  (:eval (format "%s" org-agenda-filter)))

                ;; mode-line-modes
                ;; minor-mode-alist
                " "
                mode-line-misc-info
                celestial-mode-line-string
                mode-line-end-spaces
                "%-"))

(display-time-mode)

(use-package celestial-mode-line
  :ensure t
  :if (file-exists-p (concat user-emacs-directory "secrets/secrets.el"))
  :config
  (setq calendar-location-name user-city
        calendar-longitude     user-longitude
        calendar-latitude      user-latitude)
  (celestial-mode-line-start-timer))

(provide 'mds-aesthetic-modeline)
;;; mds-aesthetic-modeline.el ends here
