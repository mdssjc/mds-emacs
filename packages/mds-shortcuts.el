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
(use-package use-package-chords
  :ensure t
  :init
  (add-hook 'after-init-hook '(lambda () (key-chord-mode t)))
  :config
  (setq key-chord-two-keys-delay 0.15
        key-chord-one-key-delay  0.15))

(use-package general
  :ensure t
  :chords
  (("qq" . save-buffers-kill-terminal)
   ("qr" . restart-emacs)
   ("xb" . ivy-switch-buffer)
   ("xs" . save-buffer)
   ("xk" . kill-this-buffer)
   ("xm" . counsel-M-x)
   ("xf" . counsel-find-file)
   ("xr" . counsel-recentf)
   ("gs" . magit-status)
   ("\\s" . embrace-commander)
   ("/s"  . embrace-commander)
   ("\\v" . hydra-expand-region/body)
   ("/v"  . hydra-expand-region/body))
  :config
  (setq super-key "<C-M-return>")
  (general-define-key :prefix super-key
                      ;; Align
                      "a a" 'align
                      "a c" 'align-current
                      "a r" 'align-regexp
                      ;; Buffer
                      "b k" 'kill-this-buffer
                      "b s" 'save-buffer
                      ;; File
                      "f l" 'find-file-literally
                      "f s" 'save-buffer
                      ;; Highlight
                      "h t h" 'hl-todo-mode
                      ;; Narrow & Widen
                      "n r" 'narrow-to-region
                      "n p" 'narrow-to-page
                      "n f" 'narrow-to-defun
                      "n w" 'widen
                      ;; Tabs
                      "t t" 'tabify
                      "t u" 'untabify)
  (general-define-key :prefix super-key :keymaps 'hl-todo-mode-map
                      "h t p" 'hl-todo-previous
                      "h t n" 'hl-todo-next
                      "h t o" 'hl-todo-occur)
  (general-define-key :keymaps 'isearch-mode-map
                      "<C-'>" 'avy-isearch)
  (general-define-key :keymaps 'selected-keymap
                      "$" 'flyspell-region
                      "C-s r" 'reverse-region
                      "C-s s" 'sort-lines
                      "C-x c" 'mds/capitalized-words
                      "C-x d" 'mds/dashed-words
                      "C-x i" 'mds/word-initials
                      "C-x l" 'mds/lower-camel-case
                      "C-x s" 'mds/snake-case
                      "C-x t" 'mds/titleized-words
                      "C-x u" 'mds/upper-camel-case
                      "C-x w" 'mds/split-words
                      "\\" 'hydra-selected/body
                      "c" 'kill-ring-save
                      "k" 'capitalize-region
                      "l" 'downcase-region
                      "m" 'apply-macro-to-region-lines
                      "p" 'yank
                      "q" 'selected-off
                      "u" 'upcase-region
                      "w" 'count-words-region
                      "x" 'kill-region)
  (general-define-key :keymaps 'selected-org-mode-map
                      "e" 'org-emphasize)
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'boon-command-map
                      "S" 'embrace-commander)
  (general-define-key
   "<C-return>"   'mds/insert-lines-above
   "<C-tab>"      'cycle-spacing
   "<M-return>"   'mds/insert-lines-below
   "<S-C-return>" 'mds/insert-lines-between
   "C-#"        'vr/query-replace
   "C-'"        'counsel-imenu
   "C-:"        'avy-goto-char-timer
   "C-="        'er/expand-region
   "C-S-f"      'swiper-multi
   "C-c /"      'counsel-rg
   "C-s"        'counsel-grep-or-swiper
   "C-x /"      'ripgrep-regexp
   "C-x C-f"    'counsel-find-file
   "C-x C-r"    'counsel-recentf
   "C-x Q q"    'save-buffers-kill-terminal
   "C-x Q r"    'restart-emacs
   "C-x S"      'embrace-commander
   "C-x V"      'hydra-expand-region/body
   "C-x g"      'magit-status
   "C-x w"      'ace-window
   "M-#"        'vr/replace
   "M-/"        'hippie-expand
   "M-<down>"   'md/move-lines-down
   "M-<up>"     'md/move-lines-up
   "M-g e"      'avy-goto-word-0
   "M-g f"      'avy-goto-line
   "M-x"        'counsel-M-x
   "M-y"        'counsel-yank-pop
   "S-C-j"      'join-line
   "S-C-s"      'swiper-all
   "S-M-<down>" 'md/duplicate-down
   "S-M-<up>"   'md/duplicate-up
   "S-M-t"      'swap-regions
   "S-SPC"      'cycle-spacing
   "s-s"        'embrace-commander
   "s-v"        'hydra-expand-region/body
   "<f12>"      'ibuffer
   ;; F5 (Toggle Global)
   "<f5> -"     'centered-cursor-mode
   "<f5> W"     'writeroom-mode
   "<f5> a"     'company-mode
   "<f5> f"     'focus-mode
   "<f5> g"     'golden-ratio-mode
   "<f5> h"     'hl-todo-mode
   "<f5> l"     'linum-mode
   "<f5> m"     'selected-global-mode
   "<f5> r"     'read-only-mode
   "<f5> t"     'toggle-truncate-lines
   "<f5> w"     'global-whitespace-mode
   ;; F7 (Aplicações Interna - Internal Applications)
   "<f7> b e"   'eww
   "<f7> e"     'eshell
   "<f7> f"     'keyfreq-show
   "<f7> p"     'projectile-command-map
   "<f7> t"     'neotree-toggle
   "<f7> u a"   'package-utils-install-async
   "<f7> u u"   'package-utils-upgrade-all
   ;; F8 (Aplicações Externa - Internal Applications)
   "<f8> g S"   'magit-stage-file
   "<f8> g g"   'magit-dispatch-popup
   "<f8> g s"   'magit-status
   "<f8> g t"   'git-timemachine-toggle
   "<f8> r"     'ripgrep-regexp)
  (key-chord-define emacs-lisp-mode-map "xe" 'eval-last-sexp)
  (defalias 'gs 'magit-status "Magit status"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode)
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-add-major-mode-key-based-replacements 'c-mode
    "C-c @" "hide blocks")
  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c C-v" "erefactor")
  (which-key-add-key-based-replacements
    "<f7> b" "browser"
    "<f7> p" "projectile"
    "<f7> t" "neotree"
    "<f7> u" "package-utils"
    "<f8> g" "magit"
    "<f8> l" "langtool"
    "<f8> n" "news"
    "<f8> r" "ripgrep"
    "<f8> s" "ispell"
    "C-c !"  "flycheck"
    "C-c &"  "yasnippet"
    "C-c p"  "projectile"
    "C-x Q"  "quit/restart"
    "M-s h"  "highlight"
    "<C-M-return> a"   "align"
    "<C-M-return> b"   "buffer"
    "<C-M-return> f"   "file"
    "<C-M-return> h"   "highlight"
    "<C-M-return> h t" "todo"
    "<C-M-return> n"   "narrow & widen"
    "<C-M-return> t"   "tabify"))

(add-to-list 'load-path (concat user-emacs-directory "temp/right-click-context"))
(use-package right-click-context
  :diminish right-click-context-mode
  :commands right-click-context-mode
  :init
  (add-hook 'after-init-hook 'right-click-context-mode))

(provide 'mds-shortcuts)
;;; mds-shortcuts.el ends here
