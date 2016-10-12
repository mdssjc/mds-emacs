;;; mds-structure.el --- Estrutura (Structure)
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
;; Conjunto de melhorias ao editor
;;  - Salva a última posição da seção;
;;  - Listagem dos documentos recentes;
;;  - Funcionalidade de reinicialização;
;;  - Visualização da árvore de modificações no documento;
;;  - Sugestão e rótulo de atalhos;
;;  - Frequência de utilização dos atalhos.
;;  - Visualizador de arquivos;
;;  - Controle de versão pelo Git;
;;  - Pacotes Abo-abo;
;;  - Seleção de partes do buffer com funcionalidades;
;;  - Navegador de projetos;
;;  - Ferramenta Ripgrep;
;;  - Busca/Substituição visual;
;;  - Browser interno.

;;; Code:
;; (require 'semantic)

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name (concat user-emacs-directory
                                                  ".cache/places")))
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name (concat user-emacs-directory
                                                    ".cache/recentf"))
        recentf-max-saved-items 1000
        recentf-max-menu-items 15)
  (recentf-mode 1))

(use-package restart-emacs
  :ensure t
  :chords
  (("qr" . restart-emacs)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

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
    "<f8> s" "ispell")
  (which-key-mode 1))

(use-package keyfreq
  :ensure t
  :bind
  (("<f7> f" . keyfreq-show))
  :init
  (setq keyfreq-file (concat user-emacs-directory
                             ".cache/.emacs.keyfreq")
        keyfreq-file-lock (concat user-emacs-directory
                                  ".cache/.emacs.keyfreq.lock"))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package neotree
  :ensure t
  :bind
  (("<f7> t" . neotree-toggle))
  :init
  (setq neo-smart-open t
        neo-mode-line-type 'neotree
        neo-show-hidden-files t
        neo-modern-sidebar t
        neo-theme 'icons))

;; Controle de Versão
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  (("<f8> g s" . magit-status)
   ("<f8> g S" . magit-stage-file)
   ("<f8> g g" . magit-dispatch-popup))
  :chords
  ("gs" . magit-status)
  :init
  (defalias 'gs 'magit-status "Magit status")
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter-fr:side 'right-fringe)
  :config
  (global-git-gutter-mode t))
;; ---

;; Abo-abo (https://github.com/abo-abo)
(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.3
        avy-background t))

(use-package ace-window
  :ensure t
  :defer 1
  :bind
  (("M-p" . ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist '((?x aw-delete-window     "Ace - Delete Window")
                            (?c aw-swap-window       "Ace - Swap Window")
                            (?n aw-flip-window)
                            (?v aw-split-window-vert "Ace - Split Vert Window")
                            (?h aw-split-window-horz "Ace - Split Horz Window")
                            (?m delete-other-windows "Ace - Maximize Window")
                            (?g delete-other-windows)
                            (?b balance-windows)
                            (?u winner-undo)
                            (?r winner-redo))
        aw-background t)
  (use-package hydra
    :config
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally  "shrink horizontal")
      ("j" shrink-window               "shrink vertical")
      ("k" enlarge-window              "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame   "new frame")
      ("x" delete-frame "delete frame"))
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" joe-scroll-other-window      "scroll")
      ("p" joe-scroll-other-window-down "scroll down"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  (ace-window-display-mode t))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :chords
  ("xn" . ivy-switch-buffer)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind
  (("C-s"   . swiper)
   ("C-S-f" . swiper-multi)))

(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /"   . counsel-ag))
  :chords
  ("xm" . counsel-M-x)
  ("xf" . counsel-find-file)
  ("xr" . counsel-recentf))

(use-package hydra
  :ensure t)
;; ---

;; Seleção
(use-package expand-region
  :ensure t
  :after hydra
  :bind
  (("C-=" . er/expand-region))
  :chords
  (("vv" . hydra-expand-region/body))
  :init
  (require 'expand-region)
  (defhydra hydra-expand-region (:columns 4 :color blue)
    "Mark"
    ("w" er/mark-word               "word")
    ("s" er/mark-symbol             "symbol")
    ("S" er/mark-symbol-with-prefix "symbol-with-prefix")
    ("n" er/mark-next-accessor      "next-accessor")
    ("m" er/mark-method-call        "method-call")
    ("i" er/mark-inside-quotes      "inside-quotes")
    ("I" er/mark-outside-quotes     "outside-quotes")
    ("p" er/mark-inside-pairs       "inside-pairs")
    ("P" er/mark-outside-pairs      "outside-pairs")
    ("c" er/mark-comment            "comment")
    ("u" er/mark-url                "url")
    ("e" er/mark-email              "email")
    ("d" er/mark-defun              "defun")
    ("q" nil                        "quit")))

(use-package embrace
  :ensure t
  :chords
  (("ss" . embrace-commander)))

(use-package selected
  :ensure t
  :commands selected-minor-mode
  :bind
  (:map selected-keymap
        ("q" . selected-off)
        ("U" . upcase-region)
        ("D" . downcase-region)
        ("W" . count-words-region)
        ("m" . apply-macro-to-region-lines)
        ("w" . mds/split-words)
        ("l" . mds/lower-camel-case)
        ("u" . mds/upper-camel-case)
        ("s" . mds/snake-case)
        ("d" . mds/dashed-words)
        ("c" . mds/capitalized-words)
        ("t" . mds/titleized-words)
        ("i" . mds/word-initials)
        :map selected-org-mode-map
        ("e" . org-emphasize))
  :init
  (defvar selected-org-mode-map (make-sparse-keymap))
  (add-hook 'activate-mark-hook   '(lambda () (selected-minor-mode t)))
  (add-hook 'deactivate-mark-hook '(lambda () (selected-minor-mode 0))))
;; ---

;; Projeto
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("<f7> p" . projectile-command-map))
  :init
  (projectile-global-mode t)
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on))
  (setq projectile-cache-file (expand-file-name (concat user-emacs-directory
                                                        ".cache/projectile.cache"))
        projectile-known-projects-file (expand-file-name (concat user-emacs-directory
                                                                 ".cache/projectile-bookmarks.eld"))
        projectile-sort-order 'modification-time
        projectile-completion-system 'ivy))
;; ---

(use-package ripgrep
  :ensure t
  :bind
  (("<f8> r" . ripgrep-regexp)))

(use-package visual-regexp
  :ensure t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

;; Browser
(use-package eww
  :commands eww eww-mode
  :bind
  (("<f7> b e" . eww))
  :config
  (setq url-configuration-directory (concat user-emacs-directory
                                            ".cache/url")))
;; ---

(provide 'mds-structure)
;;; mds-structure.el ends here
