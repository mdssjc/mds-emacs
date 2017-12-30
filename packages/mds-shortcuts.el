;;; mds-shortcuts.el --- Atalhos (Shortcuts) -*- lexical-binding: t -*-
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
                      "b n"   '(find-file         :which-key "new")
                      "b f"   '(counsel-find-file :which-key "open")
                      "b s"   '(save-buffer       :which-key "save")
                      "b S"   '(write-file        :which-key "save as")
                      "b r"   '(counsel-recentf   :which-key "recentf")
                      "b b"   '(ivy-switch-buffer :which-key "switch")
                      "b k"   '(kill-this-buffer  :which-key "kill")
                      "e"     'eshell
                      "g"     '(:which-key "magit")
                      "g S"   'magit-stage-file
                      "g g"   'magit-dispatch-popup
                      "g s"   'magit-status
                      "g t"   'git-timemachine-toggle
                      "n"     '(:which-key "news")
                      "n f"   'elfeed
                      "n t"   'twit
                      "q"     '(:which-key "quit")
                      "q q"   'save-buffers-kill-terminal
                      "q r"   'restart-emacs
                      "t"     '(:which-key "tabs")
                      "t t"   'tabify
                      "t u"   'untabify)
  ;; C-x
  (general-define-key :prefix "C-x"
                      "a"     '(:ignore t :which-key "abbrev")
                      "C-/"   'ripgrep-regexp
                      "C-r"   'recentf
                      "F"     'find-file-at-point
                      "Q"     '(:which-key "quit/restart")
                      "Q q"   'save-buffers-kill-terminal
                      "Q r"   'restart-emacs
                      "S"     'embrace-commander
                      "V"     'hydra-expand-region/body
                      "\\"    'align-regexp
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
   "<f5>"          'hydra-toggles/body
   "<mouse-9>"     'ibuffer
   "<mouse-8>"     'er/expand-region
   "C-c !"         '(:which-key "flycheck")
   "C-c &"         '(:which-key "yasnippet")
   "M-s h"         '(:which-key "highlight")
   "<f10>"         'lacarte-execute-command
   "<f12>"         'ivy-switch-buffer
   "S-<f12>"       'ibuffer
   "C-&"           'hydra-yasnippet/body
   "C-="           'er/expand-region
   "M-<mouse-1>"   'mc/add-cursor-on-click
   "C->"           'mc/mark-next-like-this
   "C-<"           'mc/mark-previous-like-this
   "C-M-<down>"    'mc/mark-next-like-this
   "C-M-<up>"      'mc/mark-previous-like-this
   "C-M-<left>"    'mc/mark-previous-like-this-symbol
   "C-M-<right>"   'mc/mark-next-like-this-symbol
   "C-M-<"         'mc/mark-all-dwim
   "C-M->"         'mc/mark-all-in-region-regexp
   "M-#"           'anzu-replace-at-cursor-thing
   "C-M-#"         'anzu-query-replace-at-cursor-thing
   "M-%"           'anzu-query-replace
   "C-M-%"         'anzu-query-replace-regexp
   "C-c /"         'counsel-grep-or-swiper
   "C-S-s"         'counsel-grep-or-swiper
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
   "S-C-f"         'swiper-multi
   "M-y"           'counsel-yank-pop
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
   "s-c i"         'ciel-ci
   "s-c o"         'ciel-co
   "s-:"           'avy-goto-char-timer
   "s-g"           'avy-goto-char-in-line
   "s-m"           'mc/mark-more-like-this-extended
   "s-s"           'embrace-commander
   "s-v"           'hydra-expand-region/body
   "s-V"           'xah-select-line)
  ;; Help
  (general-define-key :prefix "<f1>"
                      "SPC" 'counsel-find-library)
  ;; Actions
  (general-define-key :prefix "<f2>"
                      "i" 'counsel-info-lookup-symbol
                      "u" 'counsel-unicode-char)
  ;; Sintático - Syntatic
  (general-define-key :prefix "<f6>"
                      "A"   'abbrev-mode
                      "a"   'company-mode
                      "s"   '(:which-key "ispell")
                      "s p" 'ispell-pt-br
                      "s e" 'ispell-en-us
                      "s g" 'ispell-en-gb
                      "y"   'yas-minor-mode)
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
                      "SPC" 'counsel-org-goto-all
                      "a"   'org-agenda
                      "b"   'org-iswitchb
                      "c"   'org-capture
                      "g"   'org-clock-goto
                      "l"   'org-store-link
                      "p"   'org-pomodoro
                      "T"   'tomatinho)
  ;; Configurações - Configurations
  (general-define-key :keymaps 'prog-mode-map
                      "<tab>" 'company-indent-or-complete-common
                      "C-."   'counsel-dash-at-point
                      "C-,"   'zeal-at-point)
  (general-define-key :keymaps 'irony-mode-map
                      "C-c ,"     '(:ignore t :which-key "semantic")
                      "C-c @"     '(:ignore t :which-key "hide blocks")
                      "C-c C-r"   '(:ignore t :which-key "refactor")
                      "C-c C-r s" 'srefactor-refactor-at-point
                      ";"         'maio/electric-semicolon)
  (general-define-key :keymaps 'js-mode-map
                      "M-." 'nil)
  (general-define-key :keymaps 'js2-mode-map
                      ";" 'maio/electric-semicolon)
  (general-define-key :keymaps 'org-mode-map
                      "C-x x e" '(org-emphasize :which-key "Emphasize"))
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'undo-tree-map
                      "M-_" 'nil)
  (general-define-key :keymaps 'popup-isearch-keymap
                      "C-'" 'popup-isearch-cancel)
  (general-define-key :keymaps 'sql-mode-map
                      "C-c u" 'sqlup-capitalize-keywords-in-region)
  ;; Alias
  (defalias 'gs 'magit-status               "Magit status")
  (defalias 'qq 'save-buffers-kill-terminal "Quit")
  (defalias 'qr 'restart-emacs              "Restart"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0
        which-key-is-verbose t))

;; (require 'poplife)
;; (setq poplife-word-flag t)
;; (setq poplife-url-flag t)
;; (setq poplife-edit-cottager '(:imenu t :buffer t :frame t :bookmark t :recentf t))
;; (poplife-mode 1)

(use-package popup-edit-menu
  :ensure t
  :commands popup-edit-menu-stub
  :init
  (global-set-key [mouse-3] (popup-edit-menu-stub)))

(provide 'mds-shortcuts)
;;; mds-shortcuts.el ends here
