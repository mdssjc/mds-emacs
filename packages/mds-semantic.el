;;; mds-semantic.el --- Semântico (Semantic)
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
;; Analisa o texto e/ou código.

;;; Code:
;; Flycheck
(use-package flycheck
  :pin melpa
  :ensure t
  :diminish flycheck-mode " ⓢ"
  :commands flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change)
        flycheck-idle-change-delay 10
        flycheck-highlighting-mode 'lines
        flycheck-mode-line-prefix "𝓕"))

(use-package flycheck-pos-tip
  :ensure t
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5))

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;; ---

;; Flyspell
(use-package flyspell
  :ensure t
  :diminish flyspell-mode " Ⓢ"
  :commands flyspell-mode
  :init
  (setq flyspell-issue-message-flag nil)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-popup
  :ensure t
  :after flyspell)

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)
;; ---

(use-package guess-language
  :ensure t
  :commands guess-language-mode
  :init
  (add-hook 'flyspell-mode-hook 'guess-language-mode)
  :config
  (setq guess-language-languages '(en pt)
        guess-language-langcodes '((en . ("en_US" "English"))
                                   (pt . ("pt_BR" "Portuguese")))))

(use-package langtool
  :ensure t
  :commands langtool-check langtool-check-done langtool-correct-buffer langtool-switch-default-language langtool-show-message-at-point
  :config
  (setq langtool-language-tool-jar (concat user-emacs-directory "core/jars/languagetool-commandline.jar")
        langtool-default-language "pt-BR"
        langtool-mother-tongue    "pt-BR"
        langtool-autoshow-message-function (lambda (overlays)
                                             (when (require 'popup nil t)
                                               (unless (or popup-instances
                                                           (memq last-command '(keyboard-quit)))
                                                 (let ((msg (langtool-details-error-message overlays)))
                                                   (popup-tip msg)))))))

(use-package zeal-at-point
  :ensure t
  :commands zeal-at-point)

(use-package counsel-dash
  :ensure t
  :commands counsel-dash-at-point
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

(use-package eww
  :commands eww eww-mode
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="
        eww-download-directory "~/downloads"
        url-configuration-directory (concat user-emacs-directory ".cache/url")))

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :commands google-this-mode
  :init
  (setq google-this-keybind (kbd "<C-M-return> B g"))
  (add-hook 'after-init-hook 'google-this-mode))

(use-package google-translate
  :ensure t
  :after google-this
  :config
  (setq google-translate-show-phonetic t))

(use-package engine-mode
  :ensure t
  :commands engine/search-amazon engine/search-github engine/search-stack-overflow engine/search-twitter engine/search-wikipedia engine/search-wikipedia-pt engine/search-wiktionary engine/search-wiktionary-pt
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")
  (defengine github
    "https://github.com/search?q=%s")
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
