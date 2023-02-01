;; init-markup.el --- Initialize Markup Language configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 zwPapEr

;; Author: zwPapEr <zw.paper@gmail.com>
;; URL: https://github.com/zwpaper/.emacs.d

;; This file is not part of GNU Emacs.
;;
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;

;;; Commentary:
;;
;; Markup configurations.
;;

;;; Code:

(use-package graphql-mode)

(use-package obsidian
  :demand t
  :init
  (global-unset-key (kbd "M-k"))
  :config
  (obsidian-specify-path "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Papers")
  (global-obsidian-mode t)
  (bind-key (kbd "C-c M-o") 'obsidian-hydra/body 'obsidian-mode-map)

  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "clip")

  :bind
  ("M-k o" . obsidian-jump)
  ("M-k i" . obsidian-capture)
  ("M-k s" . obsidian-search)
  (:map obsidian-mode-map
        ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
        ("C-c C-o" . obsidian-follow-link-at-point)
        ;; Jump to backLinks
        ("C-c C-b" . obsidian-backlink-jump)
        ;; If you prefer you can use `obsidian-insert-link'
        ("C-c C-l" . obsidian-insert-wikilink)
        )
  )

(use-package json-mode
  :custom
  (js-indent-level 2))

(provide 'init-markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
