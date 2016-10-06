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
;;  - Visualização da árvore de modificações no documento;
;;  - Sugestão e rótulo de atalhos;
;;  - Seleção de partes do buffer
;;  - Visualizador de arquivos;
;;  - Cliente Git;
;;  - Pacotes Abo-abo: Avy, Ace-Window, Ivy, Swiper, Counsel e Hydra;
;;  - Ferramenta Ripgrep.

;;; Code:
;; (require 'semantic)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 15)
  (recentf-mode 1))

(use-package restart-emacs
  :ensure t
  :chords
  (("qq" . save-buffers-kill-terminal)
   ("qr" . restart-emacs)))

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
    "<f7> p" "projectile"
    "<f7> t" "neotree"
    "<f8> g" "magit"
    "<f8> l" "langtool"
    "<f8> r" "ripgrep"
    "<f8> s" "ispell")
  (which-key-setup-minibuffer)
  (which-key-mode 1))

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
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter-fr:side 'right-fringe)
  :config
  (global-git-gutter-mode t))

(use-package magit
  :ensure t
  :commands magit-status
  :bind
  (("<f8> g s" . magit-status)
   ("<f8> g S" . magit-stage-file)
   ("<f8> g g" . magit-dispatch-popup))
  :init
  (defalias 'gs 'magit-status "Magit status"))
;; ---

;; Abo-abo (https://github.com/abo-abo)
(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.3))
(use-package ace-window
  :ensure t
  :bind
  ("M-p" . ace-window)
  :config
  (setq aw-dispatch-always t))
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus))))
(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /" . counsel-ag)
   ("C-c l" . counsel-locate)))
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
    ("w" er/mark-word "word")
    ("s" er/mark-symbol "symbol")
    ("S" er/mark-symbol-with-prefix "symbol-with-prefix")
    ("n" er/mark-next-accessor "next-accessor")
    ("m" er/mark-method-call "method-call")
    ("i" er/mark-inside-quotes "inside-quotes")
    ("I" er/mark-outside-quotes "outside-quotes")
    ("p" er/mark-inside-pairs "inside-pairs")
    ("P" er/mark-outside-pairs "outside-pairs")
    ("c" er/mark-comment "comment")
    ("u" er/mark-url "url")
    ("e" er/mark-email "email")
    ("d" er/mark-defun "defun")
    ("q" nil "quit")))

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
  :chords
  (("xx" . selected-minor-mode))
  :init
  (defvar selected-org-mode-map (make-sparse-keymap)))
;; ---

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("<f7> p" . projectile-command-map))
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on))
  (projectile-global-mode t))

(use-package ripgrep
  :ensure t
  :bind
  (("<f8> r" . ripgrep-regexp)))

(provide 'mds-structure)
;;; mds-structure.el ends here
