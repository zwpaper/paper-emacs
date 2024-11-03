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
        ))
(package-initialize)

(load-file "~/.emacs.d/term.el")

(if (window-system)
    ;; Emacs is running in GUI mode
    (progn
      (package-refresh-contents)

      (org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
      ))

(garbage-collect)

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "http://localhost:7890")
;;         ("https" . "http://localhost:7890")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(unicode-escape fish-mode yasnippet-snippets yaml-mode which-key web-mode vterm use-package-hydra use-package-ensure-system-package tsc treemacs-projectile treemacs-icons-dired tablist shackle rust-mode rime reformatter rainbow-delimiters quelpa python-mode pretty-hydra pangu-spacing ox-hugo ox-gfm org-plus-contrib org-bullets orderless nyan-mode multiple-cursors markdown-toc magit-delta lua-mode lsp-haskell liberime kubernetes keytar json-mode indent-guide ibuffer-projectile highlight-parentheses highlight-blocks haskell-mode groovy-mode grammarly go-tag go-impl go-fill-struct format-all flycheck-posframe flycheck-popup-tip flycheck-golangci-lint flutter-l10n-flycheck exec-path-from-shell eshell-up eshell-prompt-extras eshell-did-you-mean emojify editorconfig doom-themes doom-modeline doom dockerfile-mode devdocs-browser deft dashboard dart-mode counsel corfu company-tabnine company-posframe company-box ccls aweshell all-the-icons-ivy-rich all-the-icons-ibuffer aio aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
