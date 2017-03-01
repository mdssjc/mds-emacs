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
  ;; Super-key laucher
  (general-define-key
   :prefix "<C-M-return>"
   "b"     '(:which-key "browser")
   "b e"   'eww
   "b a"   'engine/search-amazon
   "b G"   'engine/search-github
   "b g"   'engine/search-google
   "b s"   'engine/search-stack-overflow
   "b t"   'engine/search-twitter
   "b w"   'engine/search-wikipedia
   "b W"   'engine/search-wikipedia-pt
   "b d"   'engine/search-wiktionary
   "b D"   'engine/search-wiktionary-pt
   "c"     'calc
   "e"     'eshell
   "g"     '(:which-key "magit")
   "g S"   'magit-stage-file
   "g g"   'magit-dispatch-popup
   "g s"   'magit-status
   "g t"   'git-timemachine-toggle
   "n"     '(:which-key "news")
   "p"     '(projectile-command-map :which-key "projectile")
   "p s r" 'projectile-ripgrep
   "r"     'R
   "u"     '(:which-key "package-utils")
   "u a"   'package-utils-install-async
   "u u"   'package-utils-upgrade-all
   "q"     '(:which-key "quit")
   "q q"   'save-buffers-kill-terminal
   "q r"   'restart-emacs
   "t"     '(:which-key "tabs")
   "t t"   'tabify
   "t u"   'untabify)
  ;; C-x
  (general-define-key
   :prefix "C-x"
   "/"     'ripgrep-regexp
   "C-f"   'counsel-find-file
   "C-r"   'counsel-recentf
   "F"     'find-file-at-point
   "Q"     '(:which-key "quit/restart")
   "Q q"   'save-buffers-kill-terminal
   "Q r"   'restart-emacs
   "S"     'embrace-commander
   "V"     'hydra-expand-region/body
   "\\"    'align-regexp
   "g"     'magit-status
   "t"     'tabbar-ruler-move
   "x"     '(:which-key "text")
   "x a"   '(:which-key "align")
   "x a a" '(align                 :which-key "align")
   "x a c" '(align-current         :which-key "align current")
   "x a r" '(align-regexp          :which-key "align regexp")
   "x c"   '(capitalize-region     :which-key "capitalize")
   "x l"   '(downcase-region       :which-key "downcase")
   "x r"   'ciel-copy-to-register
   "x s"   '(:which-key "sorts")
   "x s P" '(sort-pages            :which-key "sort pages")
   "x s c" '(sort-columns          :which-key "sort columns")
   "x s f" '(sort-fields           :which-key "sort fields")
   "x s n" '(sort-numeric-fields   :which-key "sort numeric fields")
   "x s p" '(sort-paragraphs       :which-key "sort paragraphs")
   "x s r" '(reverse-region        :which-key "reverse lines")
   "x s r" '(sort-regexp-fields    :which-key "sort regexp fields")
   "x s s" '(sort-lines            :which-key "sort lines")
   "x t"   '(:which-key "transpose")
   "x t c" '(transpose-chars       :which-key "chars")
   "x t l" '(transpose-lines       :which-key "lines")
   "x t p" '(transpose-paragraphs  :which-key "paragraphs")
   "x t s" '(transpose-sentences   :which-key "sentences")
   "x t w" '(transpose-words       :which-key "words")
   "x u"   '(upcase-region         :which-key "upcase")
   "x w"   '(:which-key "words")
   "x w c" '(mds/capitalized-words :which-key "capitalized")
   "x w d" '(mds/dashed-words      :which-key "dashed")
   "x w i" '(mds/word-initials     :which-key "initials")
   "x w l" '(mds/lower-camel-case  :which-key "lower")
   "x w s" '(mds/snake-case        :which-key "snake")
   "x w t" '(mds/titleized-words   :which-key "titleized")
   "x w u" '(mds/upper-camel-case  :which-key "upper")
   "x w w" '(mds/split-words       :which-key "split"))
  (general-define-key
   "<f12>"         'ivy-switch-buffer
   "C-&"           'hydra-yasnippet/body
   "C-'"           'counsel-imenu
   "C-."           'counsel-dash-at-point
   "C-:"           'avy-goto-char-timer
   "C-<return>"    'mds/insert-lines-above
   "C-<tab>"       'cycle-spacing
   "C-="           'er/expand-region
   "C-M-#"         'anzu-query-replace-at-cursor-thing
   "C-M-%"         'anzu-query-replace-regexp
   "C-M-*"         'vr/query-replace
   "C-S-f"         'swiper-multi
   "C-c !"         '(:which-key "flycheck")
   "C-c &"         '(:which-key "yasnippet")
   "C-c C-/"       'counsel-rg
   "C-c I"         'emojify-insert-emoji
   "C-c p"         '(:which-key "projectile")
   "M-#"           'anzu-replace-at-cursor-thing
   "M-%"           'anzu-query-replace
   "M-*"           'vr/replace
   "M-/"           'hippie-expand
   "M-<down>"      'md/move-lines-down
   "M-<return>"    'mds/insert-lines-below
   "M-<up>"        'md/move-lines-up
   "M-g c"         'avy-goto-char
   "M-g i"         'avy-goto-char-in-line
   "M-g l"         'avy-goto-line
   "M-g r"         'avy-resume
   "M-g w"         'avy-goto-word-0
   "M-s h"         '(:which-key "highlight")
   "M-x"           'counsel-M-x
   "M-y"           'counsel-yank-pop
   "S-<f12>"       'ibuffer
   "S-C-<return>"  'mds/insert-lines-between
   "S-C-j"         'join-line
   "S-C-s"         'counsel-grep-or-swiper
   "S-M-<down>"    'md/duplicate-down
   "S-M-<up>"      'md/duplicate-up
   "S-M-t"         'swap-regions
   "S-SPC"         'cycle-spacing
   ;; Super-key hotkey
   "s-/"           'counsel-grep-or-swiper
   "s-7"           'hydra-yasnippet/body
   "s-<down>"      'md/move-lines-down
   "s-<return>"    'icy-mode
   "s-<up>"        'md/move-lines-up
   "s-C-<down>"    'enlarge-window
   "s-C-<left>"    'shrink-window-horizontally
   "s-C-<return>"  'mds/insert-lines-above
   "s-C-<right>"   'enlarge-window-horizontally
   "s-C-<up>"      'shrink-window
   "s-C-M-<down>"  'buf-move-down
   "s-C-M-<left>"  'buf-move-left
   "s-C-M-<right>" 'buf-move-right
   "s-C-M-<up>"    'buf-move-up
   "s-M-<down>"    'windmove-down
   "s-M-<left>"    'windmove-left
   "s-M-<return>"  'mds/insert-lines-below
   "s-M-<right>"   'windmove-right
   "s-M-<up>"      'windmove-up
   "s-P"           'projectile-command-map
   "s-S-<down>"    'md/duplicate-down
   "s-S-<return>"  'mds/insert-lines-between
   "s-S-<up>"      'md/duplicate-up
   "s-SPC"         'avy-goto-char-in-line
   "s-c i"         'ciel-ci
   "s-c o"         'ciel-co
   "s-p"           'projectile-speedbar-toggle
   "s-s"           'hydra-embrace/body
   "s-v"           'hydra-expand-region/body)
  ;; Toggles
  (general-define-key :prefix "<f5>"
                      "-" 'centered-cursor-mode
                      "e" 'global-emojify-mode
                      "f" 'focus-mode
                      "g" 'golden-ratio-mode
                      "h" 'hl-todo-mode
                      "l" 'linum-mode
                      "r" 'read-only-mode
                      "t" 'toggle-truncate-lines
                      "w" 'global-whitespace-mode
                      "F" 'follow-mode
                      "W" 'writeroom-mode)
  ;; Sintático - Syntatic
  (general-define-key :prefix "<f6>"
                      "A"   'abbrev-mode
                      "a"   'company-mode
                      "s"   '(:which-key "ispell")
                      "s p" 'ispell-pt-br
                      "s e" 'ispell-en-us
                      "s g" 'ispell-en-gb
                      "y"   'yas-minor-mode)
  (general-define-key :keymaps 'yas-minor-mode-map
                      "C-c & w" 'aya-create
                      "C-c & y" 'aya-expand
                      "C-c & o" 'aya-open-line)
  ;; Semântico - Semantic
  (general-define-key :prefix "<f7>"
                      "l"   '(:which-key "langtool")
                      "l c" 'langtool-check
                      "l d" 'langtool-check-done
                      "l b" 'langtool-correct-buffer
                      "l s" 'langtool-switch-default-language
                      "l ." 'langtool-show-message-at-point
                      "s"   'flycheck-mode
                      "S"   'flyspell-mode)
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
  (general-define-key :keymaps 'parinfer-mode-map
                      "C-c <return>" 'parinfer-toggle-mode)
  (general-define-key :keymaps 'emacs-lisp-mode-map
                      "<f9> t"  'litable-mode
                      "M-."     'find-function-at-point
                      "M-&"     'complete-symbol
                      "C-c e"   'macrostep-expand
                      "C-c r e" 'emr-show-refactor-menu)
  (general-define-key :keymaps racket-mode-map
                      "<f5>"     'nil
                      "M-C-<f5>" 'nil
                      "C-<f5>"   'nil
                      "C-c c"    'racket-run-and-switch-to-repl
                      "C-c C-s"  'racket-racket)
  (general-define-key :keymaps 'dumb-jump-mode-map
                      "M-g o" 'dumb-jump-go-other-window
                      "M-g j" 'dumb-jump-go
                      "M-g b" 'dumb-jump-back
                      "M-g q" 'dumb-jump-quick-look
                      "C-M-g" 'nil
                      "C-M-p" 'nil
                      "C-M-q" 'nil)
  (general-define-key :keymaps 'isearch-mode-map
                      "<C-'>" 'avy-isearch)
  (general-define-key :keymaps 'org-mode-map
                      "C-x x e" '(org-emphasize :which-key "Emphasize"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'projectile-mode-map
                      "C-c p s r" 'projectile-ripgrep)
  ;; Chords
  (key-chord-define emacs-lisp-mode-map "xe" 'eval-last-sexp)
  ;; Alias
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
    "C-x a  " "abbrev"
    "C-c p 4" "find"
    "C-c p s" "search"
    "C-c p x" "execute"
    "s-P 4  " "find"
    "s-P s  " "search"
    "s-P x  " "execute"
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
