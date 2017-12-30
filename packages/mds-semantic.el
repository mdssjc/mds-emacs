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
        flycheck-mode-line-prefix  "𝓕"))

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
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-mode-line-string   "𝓢"))

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
  (setq langtool-language-tool-jar "~/Documents/Git/languagetool/languagetool-standalone/target/LanguageTool-4.0-SNAPSHOT/LanguageTool-4.0-SNAPSHOT/languagetool-commandline.jar"
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
  :bind
  (("<f7> e" . eww))
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="
        eww-download-directory "~/downloads"
        url-configuration-directory (concat user-emacs-directory ".cache/url")))

(use-package google-this
  :ensure t
  :defer 0
  :bind
  (("<f7> g <return>" . google-this-search)
   ("<f7> g SPC"      . google-this-region)
   ("<f7> g t"        . google-this)
   ("<f7> g n"        . google-this-noconfirm)
   ("<f7> g g"        . google-this-lucky-search)
   ("<f7> g i"        . google-this-lucky-and-insert-url)
   ("<f7> g w"        . google-this-word)
   ("<f7> g s"        . google-this-symbol)
   ("<f7> g l"        . google-this-line)
   ("<f7> g e"        . google-this-error)
   ("<f7> g f"        . google-this-forecast)
   ("<f7> g r"        . google-this-cpp-reference)
   ("<f7> g m"        . google-this-maps)
   ("<f7> g a"        . google-this-ray)
   ("<f7> g m"        . google-maps)
   ("<f7> g c"        . google-this-translate-query-or-region))
  :init
  (setq google-this-keybind (kbd "<f7> g"))
  :config
  (which-key-add-prefix-title "<f7> g" "google")
  (google-this-mode))

(use-package google-translate
  :ensure t
  :defer 0
  :bind
  (("<f7> t ."   . google-translate-at-point)
   ("<f7> t RET" . google-translate-smooth-translate)
   ("<f7> t SPC" . google-translate-query-translate))
  :config
  (which-key-add-prefix-title "<f7> t" "translate")
  (setq google-translate-show-phonetic t))

(use-package engine-mode
  :ensure t
  :defer 0
  :config
  (engine/set-keymap-prefix (kbd "<f7> b"))
  (engine-mode)
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")
  (defengine github
    "https://github.com/search?q=%s"
    :keybinding "g")
  (defengine reddit
    "https://www.reddit.com/r/%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wikipedia
    "https://en.wikipedia.org/wiki/%s"
    :keybinding "w")
  (defengine wikipedia-pt
    "https://pt.wikipedia.org/wiki/%s"
    :keybinding "W")
  (defengine wiktionary
    "https://en.wiktionary.org/wiki/%s"
    :keybinding "d")
  (defengine wiktionary-pt
    "https://pt.wiktionary.org/wiki/%s"
    :keybinding "D")
  (which-key-add-key-based-replacements "<f7> b" "browser"))

(use-package lorem-ipsum
  :ensure t
  :defer t
  :bind
  (("C-x x L p" . lorem-ipsum-insert-paragraphs)
   ("C-x x L s" . lorem-ipsum-insert-sentences)
   ("C-x x L l" . lorem-ipsum-insert-list))
  :init
  (which-key-add-prefix-title "C-x x L" "lorem-ipsum"))

(provide 'mds-semantic)
;;; mds-semantic.el ends here
