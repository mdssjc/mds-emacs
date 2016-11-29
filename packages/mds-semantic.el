;;; mds-semantic.el --- Semântico (Semantic)
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
;; Analisa o texto e/ou código.

;;; Code:
(defvar langtool-path "/home/mdssjc/Documents/Git/languagetool/languagetool-standalone/target/LanguageTool-3.6-SNAPSHOT/LanguageTool-3.6-SNAPSHOT/")

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode " ⓢ"
  :pin melpa
  :bind
  (("<f5> s" . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enable
                                              save
                                              idle-change)
        flycheck-idle-change-delay 3))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))
;; ---

;; Flyspell
(use-package flyspell
  :ensure t
  :diminish flyspell-mode " Ⓢ"
  :bind
  (("<f5> S" . flyspell-mode)
   :map flyspell-mode-map
   ("C-$ TAB" . flyspell-auto-correct-word)
   ("C-$ ;" . flyspell-auto-correct-previous-word)
   ("C-$ ," . flyspell-goto-next-error)
   ("C-$ ." . flyspell-auto-correct-word)
   ("C-$ $" . flyspell-correct-word-before-point))
  :init
  (setq flyspell-auto-correct-binding nil
        flyspell-mode-map nil)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil))

(use-package flyspell-correct
  :ensure t
  :after flyspell)

(use-package flyspell-correct-popup
  :ensure t
  :after flyspell)

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

(use-package flyspell-popup
  :ensure t
  :after flyspell
  ;; :bind
  ;; (:map flyspell-mode-map
  ;;       ("C-;" . flyspell-popup-correct))
  )
;; ---

(use-package langtool
  :ensure t
  :bind
  (("<f8> l c" . langtool-check)
   ("<f8> l d" . langtool-check-done)
   ("<f8> l b" . langtool-correct-buffer)
   ("<f8> l s" . langtool-switch-default-language)
   ("<f8> l ." . langtool-show-message-at-point))
  :config
  (setq langtool-language-tool-jar (concat langtool-path "languagetool-commandline.jar")
        langtool-default-language "pt-BR"
        langtool-mother-tongue "pt-BR"
        langtool-autoshow-message-function (lambda (overlays)
                                             (when (require 'popup nil t)
                                               (unless (or popup-instances
                                                           (memq last-command '(keyboard-quit)))
                                                 (let ((msg (langtool-details-error-message overlays)))
                                                   (popup-tip msg)))))))

(defun my-eldoc-display-message (format-string &rest args)
  "Display eldoc message near point."
  (when format-string
    (pos-tip-show (apply 'format format-string args))))

(use-package eldoc
  :ensure t
  :commands eldoc-mode
  :init
  (setq eldoc-message-function 'my-eldoc-display-message))

(use-package zeal-at-point
  :ensure t
  :defer t)

(use-package counsel-dash
  :ensure t
  :bind
  (("C-." . counsel-dash-at-point))
  :preface
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point"
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))
  :config
  (setq counsel-dash-docsets-path "~/.local/share/Zeal/Zeal/docsets"
        counsel-dash-browser-func 'eww
        counsel-dash-enable-debugging nil))

(use-package engine-mode
  :ensure t
  :bind
  (("<f7> b a" . engine/search-amazon)
   ("<f7> b G" . engine/search-github)
   ("<f7> b g" . engine/search-google)
   ("<f7> b s" . engine/search-stack-overflow)
   ("<f7> b t" . engine/search-twitter)
   ("<f7> b w" . engine/search-wikipedia)
   ("<f7> b W" . engine/search-wikipedia-pt)
   ("<f7> b d" . engine/search-wiktionary)
   ("<f7> b D" . engine/search-wiktionary-pt))
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")
  (defengine github
    "https://github.com/search?q=%s")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")
  (defengine twitter
    "https://twitter.com/search?q=%s")
  (defengine wikipedia
    "https://en.wikipedia.org/wiki/%s")
  (defengine wikipedia-pt
    "https://pt.wikipedia.org/wiki/%s")
  (defengine wiktionary
    "https://en.wiktionary.org/wiki/%s")
  (defengine wiktionary-pt
    "https://pt.wiktionary.org/wiki/%s")
  (setq engine/browser-function 'eww-browse-url))

(provide 'mds-semantic)
;;; mds-semantic.el ends here
