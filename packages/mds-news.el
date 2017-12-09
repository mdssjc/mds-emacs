;;; mds-news.el --- Notícias (News) -*- lexical-binding: t -*-
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
;; Configurações para serviços de notícias em geral.

;;; Code:
(use-package wttrin
  :ensure t
  :commands wttrin
  :config
  (setq wttrin-default-cities (cons user-city nil)))

(use-package twittering-mode
  :ensure t
  :commands twit
  :bind
  (:map twittering-mode-map
        ("\\"        . hydra-twittering/body)
        ("q"         . twittering-kill-buffer)
        ("Q"         . twittering-edit-mode)
        ("j"         . twittering-goto-next-status)
        ("k"         . twittering-goto-previous-status)
        ("h"         . twittering-switch-to-next-timeline)
        ("l"         . twittering-switch-to-previous-timeline)
        ("g"         . beginning-of-buffer)
        ("G"         . end-of-buffer)
        ("t"         . twittering-update-status-interactive)
        ("X"         . twittering-delete-status)
        ("RET"       . twittering-reply-to-user)
        ("r"         . twittering-native-retweet)
        ("R"         . twittering-organic-retweet)
        ("d"         . twittering-direct-message)
        ("u"         . twittering-current-timeline)
        ("b"         . twittering-favorite)
        ("B"         . twittering-unfavorite)
        ("f"         . twittering-follow)
        ("F"         . twittering-unfollow)
        ("i"         . twittering-view-user-page)
        ("/"         . twittering-search)
        ("."         . twittering-visit-timeline)
        ("@"         . twittering-other-user-timeline)
        ("T"         . twittering-toggle-or-retrieve-replied-statuses)
        ("o"         . twittering-click)
        ("TAB"       . twittering-goto-next-thing)
        ("<backtab>" . twittering-goto-previous-thing)
        ("n"         . twittering-goto-next-status-of-user)
        ("p"         . twittering-goto-previous-status-of-user)
        ("SPC"       . twittering-scroll-up)
        ("S-SPC"     . twittering-scroll-down)
        ("y"         . twittering-push-uri-onto-kill-ring)
        ("Y"         . twittering-push-tweet-onto-kill-ring)
        ("a"         . twittering-toggle-activate-buffer))
  :init
  (add-hook 'twittering-edit-mode-hook 'flyspell-mode)
  :config
  (setq twittering-use-master-password t
        twittering-private-info-file (concat user-emacs-directory ".cache/twittering-mode.gpg")
        twittering-icon-mode t
        twittering-convert-fix-size 52
        twittering-use-icon-storage t
        twittering-icon-storage-file (concat user-emacs-directory ".cache/twittering-mode-icons.gz")
        ;; twittering-proxy-use t
        ;; twittering-proxy-server "PROXY-HOSTNAME"
        ;; twittering-proxy-port PROXY-PORT-NUMBER
        twittering-display-remaining t
        twittering-status-format "%i  %S, %RT{%FACE[bold]{%S}} %@  %FACE[shadow]{%p%f%L%r}\n%FOLD[        ]{%T}\n"
        twittering-edit-skeleton 'inherit-any
        twittering-initial-timeline-spec-string '(":home")
        twittering-timeline-header ""
        twittering-timeline-footer "")
  (defhydra hydra-twittering (:color blue :hint nil)
    "
                                                                    ╭────────────┐
     Tweets                User                        Timeline     │ Twittering │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    _k_  [_t_] post tweet      _p_  [_f_] follow                  ^_g_^      [_u_] update
    ^↑^  [_X_] delete tweet    ^↑^  [_F_] unfollow              ^_S-SPC_^    [_._] new
    ^ ^  [_r_] retweet         ^ ^  [_d_] direct message          ^^↑^^      [^@^] current user
    ^↓^  [_R_] retweet & edit  ^↓^  [_i_] profile (browser)   _h_ ←   → _l_  [_a_] toggle
    _j_  [_b_] favorite        _n_   ^ ^                          ^^↓^^
    ^ ^  [_B_] unfavorite      ^ ^   ^ ^                         ^_SPC_^
    ^ ^  [_RET_] reply         ^ ^   ^ ^                          ^_G_^
    ^ ^  [_T_] show Thread
    ^ ^  [_y_] yank url          Items                     Do
    ^ ^  [_Y_] yank tweet     ╭───────────────────────────────────────────────────────
    ^ ^  [_e_] edit mode        _<backtab>_ ← _o_pen → _<tab>_    [_q_] exit
    ^ ^   ^ ^                   ^         ^   ^ ^      ^     ^    [_/_] search
  --------------------------------------------------------------------------------
       "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("q"         twittering-kill-buffer)
    ("e"         twittering-edit-mode)
    ("j"         twittering-goto-next-status :color red)
    ("k"         twittering-goto-previous-status :color red)
    ("h"         twittering-switch-to-next-timeline :color red)
    ("l"         twittering-switch-to-previous-timeline :color red)
    ("g"         beginning-of-buffer)
    ("G"         end-of-buffer)
    ("t"         twittering-update-status-interactive)
    ("X"         twittering-delete-status)
    ("RET"       twittering-reply-to-user)
    ("r"         twittering-native-retweet)
    ("R"         twittering-organic-retweet)
    ("d"         twittering-direct-message)
    ("u"         twittering-current-timeline)
    ("b"         twittering-favorite)
    ("B"         twittering-unfavorite)
    ("f"         twittering-follow)
    ("F"         twittering-unfollow)
    ("i"         twittering-view-user-page)
    ("/"         twittering-search)
    ("."         twittering-visit-timeline)
    ("@"         twittering-other-user-timeline)
    ("T"         twittering-toggle-or-retrieve-replied-statuses)
    ("o"         twittering-click)
    ("<tab>"     twittering-goto-next-thing :color red)
    ("<backtab>" twittering-goto-previous-thing :color red)
    ("n"         twittering-goto-next-status-of-user :color red)
    ("p"         twittering-goto-previous-status-of-user :color red)
    ("SPC"       twittering-scroll-up :color red)
    ("S-SPC"     twittering-scroll-down :color red)
    ("y"         twittering-push-uri-onto-kill-ring)
    ("Y"         twittering-push-tweet-onto-kill-ring)
    ("a"         twittering-toggle-activate-buffer)))

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-use-curl t
        elfeed-feeds '(;; Blogs
                       ("https://emacsgifs.github.io/feed.xml" blog emacs)
                       ;; Emacs
                       ("http://oremacs.com/atom.xml" emacs)
                       ("http://www.lunaryorn.com/feed.atom" emacs)
                       ("http://emacsnyc.org/atom.xml" emacs)
                       ("http://emacsredux.com/atom.xml" emacs)
                       ("http://www.masteringemacs.org/feed/" emacs)
                       ("http://planet.emacsen.org/atom.xml" emacs)
                       ("http://endlessparentheses.com/atom.xml" emacs)
                       ("http://s-expressions.com/feed/" emacs)
                       ;; Clojure
                       ("http://feeds.feedburner.com/ClojureAndMe" clojure)
                       ("http://clojure.com/blog/atom.xml" clojure)
                       ("http://feeds.feedburner.com/disclojure" clojure)
                       ;; Reddit
                       ("https://www.reddit.com/r/emacs/.rss" emacs reddit))))

(provide 'mds-news)
;;; mds-news.el ends here
