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
   ("\\s" . hydra-embrace/body)
   ("/s"  . hydra-embrace/body)
   ("\\v" . hydra-expand-region/body)
   ("/v"  . hydra-expand-region/body))
  :config
  (setq super-key "<C-M-return>")
  (general-define-key :prefix super-key
                      "b"   '(:which-key "browser")
                      "b e" 'eww
                      "b a" 'engine/search-amazon
                      "b G" 'engine/search-github
                      "b g" 'engine/search-google
                      "b s" 'engine/search-stack-overflow
                      "b t" 'engine/search-twitter
                      "b w" 'engine/search-wikipedia
                      "b W" 'engine/search-wikipedia-pt
                      "b d" 'engine/search-wiktionary
                      "b D" 'engine/search-wiktionary-pt
                      "c"   'calc
                      "e"   'eshell
                      "g"   '(:which-key "magit")
                      "g S" 'magit-stage-file
                      "g g" 'magit-dispatch-popup
                      "g s" 'magit-status
                      "g t" 'git-timemachine-toggle
                      "n"   '(:which-key "news")
                      "p"   '(projectile-command-map :which-key "projectile")
                      "p s r" 'projectile-ripgrep
                      "r"   'R
                      "u"   '(:which-key "package-utils")
                      "u a" 'package-utils-install-async
                      "u u" 'package-utils-upgrade-all
                      "q"   '(:which-key "quit")
                      "q q" 'save-buffers-kill-terminal
                      "q r" 'restart-emacs
                      ;; Tabs
                      "t"   '(:which-key "tabs")
                      "t t" 'tabify
                      "t u" 'untabify)
  (general-define-key :keymaps 'isearch-mode-map
                      "<C-'>" 'avy-isearch)
  (general-define-key :keymaps 'org-mode-map
                      "C-x x e" '(org-emphasize :which-key "Emphasize"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'projectile-mode-map
                      "C-c p s r" 'projectile-ripgrep)
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
   "C-."        'counsel-dash-at-point
   "C-="        'er/expand-region
   "C-S-f"      'swiper-multi
   "C-c !"      '(:which-key "flycheck")
   "C-c &"      '(:which-key "yasnippet")
   "C-c /"      'counsel-rg
   "C-c I"      'emojify-insert-emoji
   "C-c p"      '(:which-key "projectile")
   "C-x /"      '(:which-key "search")
   "C-x / g"    'google-this
   "C-x / r"    'ripgrep-regexp
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
   "s-s"        'hydra-embrace/body
   "s-c i"      'ciel-ci
   "s-c o"      'ciel-co
   "s-v"        'hydra-expand-region/body
   "s-P"        'projectile-command-map
   "s-p"        'projectile-speedbar-open-current-buffer-in-tree
   "<s-return>" 'icy-mode
   "s-C-<up>"    'shrink-window
   "s-C-<down>"  'enlarge-window
   "s-C-<left>"  'shrink-window-horizontally
   "s-C-<right>" 'enlarge-window-horizontally
   "s-M-<up>"    'windmove-up
   "s-M-<down>"  'windmove-down
   "s-M-<left>"  'windmove-left
   "s-M-<right>" 'windmove-right
   "s-C-M-<up>"    'buf-move-up
   "s-C-M-<down>"  'buf-move-down
   "s-C-M-<left>"  'buf-move-left
   "s-C-M-<right>" 'buf-move-right
   "s-C-<return>" 'mds/insert-lines-above
   "s-M-<return>" 'mds/insert-lines-below
   "s-S-<return>" 'mds/insert-lines-between
   "<f12>"      'ivy-switch-buffer
   "S-<f12>"    'ibuffer)
  ;; Toggles
  (general-define-key :prefix "<f5>"
                      "-" 'centered-cursor-mode
                      "e" 'global-emojify-mode
                      "f" 'focus-mode
                      "g" 'golden-ratio-mode
                      "h" 'hl-todo-mode
                      "l" 'linum-mode
                      "m" 'selected-global-mode
                      "r" 'read-only-mode
                      "t" 'toggle-truncate-lines
                      "w" 'global-whitespace-mode
                      "F" 'follow-mode
                      "W" 'writeroom-mode)
  ;; Sintático - Syntatic
  (general-define-key :prefix "<f6>"
                      "A" 'abbrev-mode
                      "a" 'company-mode
                      "s" '(:which-key "ispell")
                      "s p" 'ispell-pt-br
                      "s e" 'ispell-en-us
                      "s g" 'ispell-en-gb
                      "y" 'yas-minor-mode)
  (general-define-key :keymaps 'yas-minor-mode-map
                      "C-c & w" 'aya-create
                      "C-c & y" 'aya-expand
                      "C-c & o" 'aya-open-line)
  ;; Semântico - Semantic
  (general-define-key :prefix "<f7>"
                      "l" '(:which-key "langtool")
                      "l c" 'langtool-check
                      "l d" 'langtool-check-done
                      "l b" 'langtool-correct-buffer
                      "l s" 'langtool-switch-default-language
                      "l ." 'langtool-show-message-at-point
                      "s" 'flycheck-mode
                      "S" 'flyspell-mode)
  (general-define-key :keymaps 'flyspell-mode-map
                      "C-$"   'flyspell-popup-correct
                      "C-M-$" 'flyspell-correct-word-generic
                      "C-M-i" 'nil
                      "C-TAB" 'nil
                      "C-;"   'nil
                      "C-,"   'nil
                      "C-."   'nil)
  ;; Pragmático - Pragmatic
  (general-define-key :prefix "<f8>"
                      "l" 'org-store-link
                      "a" 'org-agenda
                      "c" 'org-capture
                      "b" 'org-iswitchb
                      "p" 'org-pomodoro
                      "T" 'tomatinho)
  ;; Configurações - Configurations
  (general-define-key :prefix "<f9>")
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
    "C-x a   " "abbrev"
    "C-c p 4 " "find"
    "C-c p s " "search"
    "C-c p x " "execute"
    "s-P 4   " "find"
    "s-P s   " "search"
    "s-P x   " "execute"
    "<C-M-return> p 4" "find"
    "<C-M-return> p s" "search"
    "<C-M-return> p x" "execute")
  (which-key-add-major-mode-key-based-replacements 'c-mode
    "C-c ," "semantic"
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
