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
  (("qq" . save-buffers-kill-terminal)
   ("xs" . save-buffer)
   ("xk" . kill-this-buffer))
  :config
  (setq super-key "<C-M-return>")
  (general-define-key :prefix super-key
                      "a a" 'align
                      "a c" 'align-current
                      "a r" 'align-regexp)
  (general-define-key
   "<f5> l"     'linum-mode
   "M-<up>"     'mds/move-up
   "M-<down>"   'mds/move-down
   "M-S-<up>"   'mds/duplicate-up
   "M-S-<down>" 'mds/duplicate-down
   "<C-tab>"    'cycle-spacing
   ;; "<C-return>" 'mds/insert-lines-between
   "M-/"        'hippie-expand
   "<f5> w"     'global-whitespace-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-add-key-based-replacements
    "<f7> b" "browser"
    "<f7> p" "projectile"
    "<f7> t" "neotree"
    "<f8> g" "magit"
    "<f8> l" "langtool"
    "<f8> r" "ripgrep"
    "<f8> s" "ispell"
    "C-c /"  "counsel-rg"
    "<C-M-return> a" "align")
  (which-key-mode 1))

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

(use-package writeroom-mode
  :ensure t
  :bind
  (("<f5> W" . writeroom-mode)))

(use-package focus
  :ensure t
  :bind
  (("<f5> f" . focus-mode)))

(provide 'mds-shortcuts)
;;; mds-shortcuts ends here
