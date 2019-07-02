;;; package --- Summary
;;; This is zwPapEr`s personal init.el file for EMACS
;;; Commentary:

;; Loads the README org file which contains the *REAL* meat

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ))
(package-initialize)
(package-refresh-contents)

(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(garbage-collect)

(provide 'init)
;;; init.el ends here
