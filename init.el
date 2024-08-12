;;; package --- Summary
;;; This is zwPapEr`s personal init.el file for EMACS
;;; Commentary:

;; Loads the README org file which contains the *REAL* meat

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq comp-deferred-compilation t)

(require 'package)
(setq package-archives
      '(
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ;; ("org" . "http://elpa.emacs-china.org/org/")
        ))
(add-to-list 'package-pinned-packages '(telega . "melpa-stable"))

(setq url-proxy-services '(("no_proxy" . "\\(localhost\\|tsinghua\\.edu\\.cn\\|192\\.168\\..*\\|10\\..*\\|172\\..*\\|sensetime\\.com\\)")
                           ("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")))
(package-initialize)
;; (package-refresh-contents)

;; only load complicated config in GUI mode, for terminal mode, use simple config
(load-file "~/.emacs.d/term.el")
(when (display-graphic-p)
  (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
  )

(garbage-collect)

(provide 'init)
;;; init.el ends here
