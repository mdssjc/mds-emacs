;;; mds-shortcuts.el --- Atalhos (Shortcuts)
;;
;; Copyright (C) 2016-2017 Marcelo dos Santos
;;
;; author: Marcelo dos Santos <mds>
;; URL: https://github.com/mdssjc/mds-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

;;; Commentary:
;; Atalhos do ambiente Emacs.

;;; Code:
(use-package general
  :ensure t
  :config
  ;; Super-key laucher
  (general-define-key :prefix "<C-M-return>"
                      "a"     '(:which-key "applications")
                      "a c"   'calc
                      "a e"   'esup
                      "a r"   'R
                      "a s"   'symon-mode
                      "a w"   'wttrin
                      "b"     '(:which-key "buffer")
                      "b b"   'ivy-switch-buffer
                      "b f"   'counsel-find-file
                      "b k"   'kill-this-buffer
                      "b r"   'counsel-recentf
                      "b s"   'save-buffer
                      "B"     '(:which-key "browser")
                      "B e"   'eww
                      "B a"   'engine/search-amazon
                      "B G"   'engine/search-github
                      "B g"   'engine/search-google
                      "B s"   'engine/search-stack-overflow
                      "B t"   'engine/search-twitter
                      "B w"   'engine/search-wikipedia
                      "B W"   'engine/search-wikipedia-pt
                      "B d"   'engine/search-wiktionary
                      "B D"   'engine/search-wiktionary-pt
                      "e"     'eshell
                      "g"     '(:which-key "magit")
                      "g S"   'magit-stage-file
                      "g g"   'magit-dispatch-popup
                      "g s"   'magit-status
                      "g t"   'git-timemachine-toggle
                      "n"     '(:which-key "news")
                      "n f"   'elfeed
                      "n t"   'twit
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
  (general-define-key :prefix "C-x"
                      "a"     '(:ignore t :which-key "abbrev")
                      "/"     'rg
                      "C-/"   'ripgrep-regexp
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
   "C-c /"         '(:which-key "google")
   "C-c !"         '(:which-key "flycheck")
   "C-c &"         '(:which-key "yasnippet")
   "M-s h"         '(:which-key "highlight")
   "<f10>"         'lacarte-execute-command
   "<f12>"         'ivy-switch-buffer
   "S-<f12>"       'ibuffer
   "C-&"           'hydra-yasnippet/body
   "C-="           'er/expand-region
   "C->"           'mc/mark-next-like-this
   "C-<"           'mc/mark-previous-like-this
   "C-M-."         'mc/mark-all-dwim
   "C-M-,"         'mc/mark-all-in-region-regexp
   "M-#"           'anzu-replace-at-cursor-thing
   "C-M-#"         'anzu-query-replace-at-cursor-thing
   "M-%"           'anzu-query-replace
   "C-M-%"         'anzu-query-replace-regexp
   "M-*"           'vr/replace
   "C-M-*"         'vr/query-replace
   "C-c / ."       'google-translate-at-point
   "C-c / S"       'google-translate-smooth-translate
   "C-c / T"       'google-translate-query-translate
   "C-c C-/"       'counsel-rg
   "C-c I"         'emojify-insert-emoji
   "C-:"           'avy-goto-char-timer
   "M-g i"         'avy-goto-char-in-line
   "M-g l"         'avy-goto-line
   "M-g r"         'avy-resume
   "M-+"           'shift-number-up
   "M-_"           'shift-number-down
   "M-/"           'hippie-expand
   "M-Z"           'avy-goto-char-in-line
   "S-C-j"         'join-line
   "C-'"           'popup-imenu
   "C-."           'counsel-dash-at-point
   "S-C-f"         'swiper-multi
   "S-C-s"         'counsel-grep-or-swiper
   "M-x"           'counsel-M-x
   "M-y"           'counsel-yank-pop
   "S-M-t"         'swap-regions
   "S-SPC"         'cycle-spacing
   "M-<up>"        'md/move-lines-up
   "M-<down>"      'md/move-lines-down
   "S-M-<up>"      'md/duplicate-up
   "S-M-<down>"    'md/duplicate-down
   "C-<return>"    'mds/insert-lines-above
   "S-C-<return>"  'mds/insert-lines-between
   "M-<return>"    'mds/insert-lines-below
   ;; Super-key hotkey
   "s-<return>"    'icy-mode
   "s-<up>"        'md/move-lines-up
   "s-<down>"      'md/move-lines-down
   "s-S-<up>"      'md/duplicate-up
   "s-S-<down>"    'md/duplicate-down
   "s-C-<return>"  'mds/insert-lines-above
   "s-S-<return>"  'mds/insert-lines-between
   "s-M-<return>"  'mds/insert-lines-below
   "s-C-<up>"      'shrink-window
   "s-C-<down>"    'enlarge-window
   "s-C-<left>"    'shrink-window-horizontally
   "s-C-<right>"   'enlarge-window-horizontally
   "s-M-<up>"      'windmove-up
   "s-M-<down>"    'windmove-down
   "s-M-<left>"    'windmove-left
   "s-M-<right>"   'windmove-right
   "s-C-M-<up>"    'buf-move-up
   "s-C-M-<down>"  'buf-move-down
   "s-C-M-<left>"  'buf-move-left
   "s-C-M-<right>" 'buf-move-right
   "s-7"           'hydra-yasnippet/body
   "s-/"           'counsel-grep-or-swiper
   "s-c i"         'ciel-ci
   "s-c o"         'ciel-co
   "s-."           'mc/mark-all-dwim
   "s-,"           'mc/mark-all-in-region-regexp
   "s-:"           'avy-goto-char-timer
   "s-g"           'avy-goto-char-in-line
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
  (general-define-key :keymaps 'prog-mode-map
                      "<tab>" 'company-indent-or-complete-common)
  (general-define-key :keymaps 'parinfer-mode-map
                      "C-c <tab>" 'parinfer-toggle-mode)
  (general-define-key :keymaps 'emacs-lisp-mode-map
                      "<f9> p"    'parinfer-mode
                      "<f9> P"    'enable-paredit-mode
                      "<f9> l"    'lispy-mode
                      "<f9> r"    'rainbow-delimiters-mode
                      "<f9> t"    'litable-mode
                      "M-."       'find-function-at-point
                      "M-&"       'complete-symbol
                      "C-c e"     'macrostep-expand
                      "C-c C-r"   '(:ignore t :which-key "refactor")
                      "C-c C-r v" '(:ignore t :which-key "erefactor")
                      "C-c C-r e" 'emr-show-refactor-menu)
  (general-define-key :keymaps 'racket-mode-map
                      "<f9> p"   'parinfer-mode
                      "<f9> P"   'enable-paredit-mode
                      "<f9> l"   'lispy-mode
                      "<f9> r"   'rainbow-delimiters-mode
                      "<f9> t"   'litable-mode
                      "<f5>"     'nil
                      "M-C-<f5>" 'nil
                      "C-<f5>"   'nil
                      "C-c c"    'racket-run-and-switch-to-repl
                      "C-c C-s"  'racket-racket)
  (general-define-key :keymaps 'c-mode-map
                      "C-c ,"     '(:ignore t :which-key "semantic")
                      "C-c @"     '(:ignore t :which-key "hide blocks")
                      "C-c C-r"   '(:ignore t :which-key "refactor")
                      "C-c C-r e" 'emr-show-refactor-menu
                      "C-c C-r s" 'srefactor-refactor-at-point
                      ";"         'maio/electric-semicolon)
  (general-define-key :keymaps 'dumb-jump-mode-map
                      "M-g o" 'dumb-jump-go-other-window
                      "M-g j" 'dumb-jump-go
                      "M-g b" 'dumb-jump-back
                      "M-g q" 'dumb-jump-quick-look
                      "C-M-g" 'nil
                      "C-M-p" 'nil
                      "C-M-q" 'nil)
  (general-define-key :keymaps 'org-mode-map
                      "C-x x e" '(org-emphasize :which-key "Emphasize"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'projectile-mode-map
                      "<C-M-return> p"     '(projectile-command-map :which-key "projectile")
                      "<C-M-return> p 4"   '(projectile-command-map :which-key "find")
                      "<C-M-return> p s"   '(projectile-command-map :which-key "search")
                      "<C-M-return> p x"   '(projectile-command-map :which-key "execute")
                      "<C-M-return> p s r" 'projectile-ripgrep
                      "C-c p"     '(:ignore t :which-key "projectile")
                      "C-c p 4"   '(:ignore t :which-key "find")
                      "C-c p s"   '(:ignore t :which-key "search")
                      "C-c p x"   '(:ignore t :which-key "execute")
                      "C-c p s r" 'projectile-ripgrep
                      "s-p"   'projectile-command-map
                      "s-p 4" '(:ignore t :which-key "find")
                      "s-p s" '(:ignore t :which-key "search")
                      "s-p x" '(:ignore t :which-key "execute")
                      "s-P"   'projectile-speedbar-toggle
                      "M-SPC" 'counsel-projectile-drop-to-switch-project)
  (general-define-key :keymaps 'twittering-mode-map
                      "\\"        'hydra-twittering/body
                      "q"         'twittering-kill-buffer
                      "Q"         'twittering-edit-mode
                      "j"         'twittering-goto-next-status
                      "k"         'twittering-goto-previous-status
                      "h"         'twittering-switch-to-next-timeline
                      "l"         'twittering-switch-to-previous-timeline
                      "g"         'beginning-of-buffer
                      "G"         'end-of-buffer
                      "t"         'twittering-update-status-interactive
                      "X"         'twittering-delete-status
                      "RET"       'twittering-reply-to-user
                      "r"         'twittering-native-retweet
                      "R"         'twittering-organic-retweet
                      "d"         'twittering-direct-message
                      "u"         'twittering-current-timeline
                      "b"         'twittering-favorite
                      "B"         'twittering-unfavorite
                      "f"         'twittering-follow
                      "F"         'twittering-unfollow
                      "i"         'twittering-view-user-page
                      "/"         'twittering-search
                      "."         'twittering-visit-timeline
                      "@"         'twittering-other-user-timeline
                      "T"         'twittering-toggle-or-retrieve-replied-statuses
                      "o"         'twittering-click
                      "TAB"       'twittering-goto-next-thing
                      "<backtab>" 'twittering-goto-previous-thing
                      "n"         'twittering-goto-next-status-of-user
                      "p"         'twittering-goto-previous-status-of-user
                      "SPC"       'twittering-scroll-up
                      "S-SPC"     'twittering-scroll-down
                      "y"         'twittering-push-uri-onto-kill-ring
                      "Y"         'twittering-push-tweet-onto-kill-ring
                      "a"         'twittering-toggle-activate-buffer)
  (general-define-key :keymaps 'elfeed-show-mode-map
                      "j" 'next-line
                      "k" 'previous-lin
                      "u" 'elfeed-update)
  (general-define-key :keymaps 'web-mode-map
                      "<f9> p" 'emmet-preview-mode)
  (general-define-key :keymaps 'emmet-mode-keymap
                      "C-<return>" 'nil)
  (general-define-key :keymaps 'haskell-mode-map
                      "<f9> s"    'structured-haskell-mode
                      "M-<right>" 'haskell-move-nested-right
                      "M-<left>"  'haskell-move-nested-left
                      "C-c ."     'counsel-dash-at-point
                      ;; Debug
                      "C-c d a"   'haskell-debug/abandon
                      "C-c d b"   'haskell-debug/break-on-function
                      "C-c d B"   'haskell-debug/delete
                      "C-c d c"   'haskell-debug/continue
                      "C-c d d"   'haskell-debug
                      "C-c d n"   'haskell-debug/next
                      "C-c d N"   'haskell-debug/previous
                      "C-c d p"   'haskell-debug/previous
                      "C-c d r"   'haskell-debug/refresh
                      "C-c d s"   'haskell-debug/step
                      "C-c d t"   'haskell-debug/trace
                      ;; Editing
                      "C-c e j"   'haskell-navigate-imports
                      "C-c e f"   'haskell-mode-format-imports
                      "C-c e s"   'haskell-sort-imports
                      "C-c e a"   'haskell-align-imports
                      "C-c e S"   'haskell-mode-stylish-haskell
                      ;; Compilation
                      "C-c c"     'haskell-compile
                      ;; Interpreter
                      "C-c '"     'haskell-interactive-bring
                      "C-c i z"   'switch-to-haskell
                      "C-c i b"   'switch-to-haskell
                      "C-c i l"   'inferior-haskell-load-file
                      "C-c i t"   'inferior-haskell-type
                      "C-c i i"   'inferior-haskell-info
                      "C-c i d"   'inferior-haskell-find-definition
                      "C-c i c"   'haskell-interactive-mode-clear
                      ;; Lookup
                      "C-c l t"   'haskell-process-do-type
                      "C-c l i"   'haskell-process-do-info
                      ;; Refactor - Hare
                      "C-c r d m" 'hare-refactor-demote
                      "C-c r d d" 'hare-refactor-dupdef
                      "C-c r i c" 'hare-refactor-iftocase
                      "C-c r l o" 'hare-refactor-lift-one
                      "C-c r l t" 'hare-refactor-lifttotop
                      "C-c r r"   'hare-refactor-rename
                      "C-c r t"   'hare-refactor-roundtrip
                      "C-c r s h" 'hare-refactor-show)
  (general-define-key :keymaps 'undo-tree-map
                      "M-_" 'nil)
  (general-define-key :keymaps 'java-mode-map
                      "<f9> j"  'jdee-mode
                      "<f9> m"  'meghanada-mode
                      "C-c C-c" '(:ignore t :which-key "compile")
                      "C-c C-r" '(:ignore t :which-key "refactor")
                      "C-c C-v" '(:ignore t :which-key "project"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'sql-mode-map
                      "C-c u" 'sqlup-capitalize-keywords-in-region)
  ;; Alias
  (defalias 'gs 'magit-status "Magit status"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode)
  :config
  (setq which-key-idle-delay 0))

(use-package popup-edit-menu
  :ensure t
  :config
  (global-set-key [mouse-3] (popup-edit-menu-stub)))

(provide 'mds-shortcuts)
;;; mds-shortcuts.el ends here
