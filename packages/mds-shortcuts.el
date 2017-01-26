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
   ("\\7" . hydra-yasnippet/body)
   ("/7"  . hydra-yasnippet/body)
   ("\\g" . avy-goto-char-in-line)
   ("/g"  . avy-goto-char-in-line)
   ("\\s" . embrace-commander)
   ("/s"  . embrace-commander)
   ("\\v" . hydra-expand-region/body)
   ("/v"  . hydra-expand-region/body)
   ("\\w" . ace-window)
   ("/w"  . ace-window))
  :config
  (setq super-key "<C-M-return>")
  (general-define-key :prefix super-key
                      ;; Buffer
                      "b"   '(:which-key "buffer")
                      "b b" 'ivy-switch-buffer
                      "b k" 'kill-this-buffer
                      "b s" 'save-buffer
                      ;; File
                      "f"   '(:which-key "file")
                      "f f" 'counsel-find-file
                      "f l" 'find-file-literally
                      "f r" 'counsel-recentf
                      "f s" 'save-buffer
                      ;; Highlight
                      "h"     '(:which-key "highlight")
                      "h t"   '(:which-key "todo")
                      "h t h" 'hl-todo-mode
                      ;; Tabs
                      "t"   '(:which-key "tabs")
                      "t t" 'tabify
                      "t u" 'untabify)
  (general-define-key :prefix super-key :keymaps 'hl-todo-mode-map
                      "h t p" 'hl-todo-previous
                      "h t n" 'hl-todo-next
                      "h t o" 'hl-todo-occur)
  (general-define-key :keymaps 'isearch-mode-map
                      "<C-'>" 'avy-isearch)
  (general-define-key :keymaps 'org-mode-map
                      "C-x x e" '(org-emphasize :which-key "Emphasize"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'boon-command-map
                      "S" 'embrace-commander)
  (general-define-key :keymaps 'yas-minor-mode-map
                      "C-c & w" 'aya-create
                      "C-c & y" 'aya-expand
                      "C-c & o" 'aya-open-line)
  (general-define-key
   "<C-return>"   'mds/insert-lines-above
   "<C-tab>"      'cycle-spacing
   "<M-return>"   'mds/insert-lines-below
   "<S-C-return>" 'mds/insert-lines-between
   "C-x x"      '(:which-key "text")
   "C-x x c"    '(capitalize-region :which-key "capitalize")
   "C-x x l"    '(downcase-region   :which-key "downcase")
   "C-x x u"    '(upcase-region     :which-key "upcase")
   "C-x x a"    '(:which-key "align")
   "C-x x a a"  '(align :which-key "align")
   "C-x x a c"  '(align-current :which-key "align current")
   "C-x x a r"  '(align-regexp  :which-key "align regexp")
   "C-x x i"    'ciel-ci
   "C-x x o"    'ciel-co
   "C-x x r"    'ciel-copy-to-register
   "C-x x s"    '(:which-key "sorts")
   "C-x x s P"  '(sort-pages   :which-key "sort pages")
   "C-x x s c"  '(sort-columns :which-key "sort columns")
   "C-x x s f"  '(sort-fields  :which-key "sort fields")
   "C-x x s n"  '(sort-numeric-fields :which-key "sort numeric fields")
   "C-x x s p"  '(sort-paragraphs     :which-key "sort paragraphs")
   "C-x x s r"  '(reverse-region      :which-key "reverse lines")
   "C-x x s r"  '(sort-regexp-fields  :which-key "sort regexp fields")
   "C-x x s s"  '(sort-lines          :which-key "sort lines")
   "C-x x t"    '(:which-key "transpose")
   "C-x x t c"  '(transpose-chars :which-key "chars")
   "C-x x t l"  '(transpose-lines :which-key "lines")
   "C-x x t p"  '(transpose-paragraphs :which-key "paragraphs")
   "C-x x t s"  '(transpose-sentences  :which-key "sentences")
   "C-x x t w"  '(transpose-words :which-key "words")
   "C-x x w"    '(:which-key "words")
   "C-x x w c"  '(mds/capitalized-words :which-key "capitalized")
   "C-x x w d"  '(mds/dashed-words      :which-key "dashed")
   "C-x x w i"  '(mds/word-initials     :which-key "initials")
   "C-x x w l"  '(mds/lower-camel-case  :which-key "lower")
   "C-x x w s"  '(mds/snake-case        :which-key "snake")
   "C-x x w t"  '(mds/titleized-words   :which-key "titleized")
   "C-x x w u"  '(mds/upper-camel-case  :which-key "upper")
   "C-x x w w"  '(mds/split-words       :which-key "split")
   "C-&"        'hydra-yasnippet/body
   "C-'"        'counsel-imenu
   "C-:"        'avy-goto-char-timer
   "C-="        'er/expand-region
   "C-S-f"      'swiper-multi
   "C-c !"      '(:which-key "flycheck")
   "C-c &"      '(:which-key "yasnippet")
   "C-c /"      'counsel-rg
   "C-c I"      'emojify-insert-emoji
   "C-c p"      '(:which-key "projectile")
   "C-x /"      'ripgrep-regexp
   "C-x \\"     'align-regexp
   "C-x C-f"    'counsel-find-file
   "C-x C-r"    'counsel-recentf
   "C-x F"      'find-file-at-point
   "C-x Q"      '(:which-key "quit/restart")
   "C-x Q q"    'save-buffers-kill-terminal
   "C-x Q r"    'restart-emacs
   "C-x S"      'embrace-commander
   "C-x V"      'hydra-expand-region/body
   "C-x g"      'magit-status
   "M-/"        'hippie-expand
   "M-<down>"   'md/move-lines-down
   "M-<up>"     'md/move-lines-up
   "M-g c"      'avy-goto-char
   "M-g i"      'avy-goto-char-in-line
   "M-g l"      'avy-goto-line
   "M-g r"      'avy-resume
   "M-g w"      'avy-goto-word-0
   "M-s h"      '(:which-key "highlight")
   "M-x"        'counsel-M-x
   "M-y"        'counsel-yank-pop
   "S-C-j"      'join-line
   "S-C-s"      'counsel-grep-or-swiper
   "S-M-<down>" 'md/duplicate-down
   "S-M-<up>"   'md/duplicate-up
   "S-M-t"      'swap-regions
   "S-SPC"      'cycle-spacing
   "s-/"        'counsel-grep-or-swiper
   "s-7"        'hydra-yasnippet/body
   "s-<down>"   'md/move-lines-down
   "s-<up>"     'md/move-lines-up
   "s-S-<down>" 'md/duplicate-down
   "s-S-<up>"   'md/duplicate-up
   "s-SPC"      'avy-goto-char-in-line
   "s-s"        'embrace-commander
   "s-v"        'hydra-expand-region/body
   "s-w"        'ace-window
   "<f12>"      'ivy-switch-buffer
   "S-<f12>"    'ibuffer
   ;; F5 (Toggle Global)
   "<f5> -"     'centered-cursor-mode
   "<f5> W"     'writeroom-mode
   "<f5> a"     'company-mode
   "<f5> e"     'global-emojify-mode
   "<f5> f"     'focus-mode
   "<f5> g"     'golden-ratio-mode
   "<f5> h"     'hl-todo-mode
   "<f5> l"     'linum-mode
   "<f5> m"     'selected-global-mode
   "<f5> r"     'read-only-mode
   "<f5> t"     'toggle-truncate-lines
   "<f5> w"     'global-whitespace-mode
   ;; F7 (Aplicações Interna - Internal Applications)
   "<f7> b"     '(:which-key "browser")
   "<f7> b e"   'eww
   "<f7> e"     'eshell
   "<f7> f"     'keyfreq-show
   "<f7> p"     '(projectile-command-map :which-key "projectile")
   "<f7> t"     '(neotree-toggle :which-key "neotree")
   "<f7> u"     '(:which-key "package-utils")
   "<f7> u a"   'package-utils-install-async
   "<f7> u u"   'package-utils-upgrade-all
   ;; F8 (Aplicações Externa - Internal Applications)
   "<f8> g"     '(:which-key "magit")
   "<f8> g S"   'magit-stage-file
   "<f8> g g"   'magit-dispatch-popup
   "<f8> g s"   'magit-status
   "<f8> g t"   'git-timemachine-toggle
   "<f8> l"     '(:which-key "langtool")
   "<f8> n"     '(:which-key "news")
   "<f8> r"     '(ripgrep-regexp :which-key "ripgrep")
   "<f8> s"     '(:which-key "ispell")
   "<f8> s p"   'ispell-pt-br
   "<f8> s e"   'ispell-en-us
   "<f8> s g"   'ispell-en-gb)
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
  (which-key-add-key-based-replacements
    "C-x a" "abbrev")
  (which-key-add-major-mode-key-based-replacements 'c-mode
    "C-c ," "semantic"
    "C-c ." "ede"
    "C-c @" "hide blocks"
    "C-c r" "refactor")
  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c r v" "erefactor"
    "C-c r"   "refactor"))

(add-to-list 'load-path (concat user-emacs-directory "temp/right-click-context"))
(use-package right-click-context
  :diminish right-click-context-mode
  :commands right-click-context-mode
  :init
  (add-hook 'after-init-hook 'right-click-context-mode))

(provide 'mds-shortcuts)
;;; mds-shortcuts.el ends here
