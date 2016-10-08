;;; mds-shortcuts.el --- Atalhos (Shortcuts)
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
;; Atalhos do ambiente.

;;; Code:
(use-package general
  :ensure t
  :chords
  (("qq" . save-buffers-kill-terminal))
  :config
  (general-define-key
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   "<C-return>" 'mds/insert-lines-between
   "M-/"        'hippie-expand))

(use-package golden-ratio
  :ensure t
  :diminish " φ"
  :bind
  (("<f5> g" . golden-ratio-mode)))

(use-package centered-cursor-mode
  :ensure t
  :diminish " ⊝"
  :bind
  (("<f5> -" . centered-cursor-mode)))

(use-package boon
  :ensure t
  :bind
  (("<f5> b" . boon-mode))
  :config
  (require 'boon-colemak)
  (add-to-list 'boon-special-mode-list 'emacs-lisp-mode))

(use-package writeroom-mode
  :ensure t
  :bind
  (("<f5> w" . writeroom-mode)))

(provide 'mds-shortcuts)
;;; mds-shortcuts ends here
