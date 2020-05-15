;;; package --- Summary
;; C++, C, Rust
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package init-lsp
  :ensure nil)

;; YASnippet
(use-package yasnippet
  :init
  (use-package yasnippet-snippets)
  :config
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
  (yas-global-mode 1))


(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-golangci-lint
    :hook (go-mode . flycheck-golangci-lint-setup))
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :hook (flycheck-mode . flycheck-posframe-mode)
            :config
            (flycheck-posframe-configure-pretty-defaults))

        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(use-package lsp-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :commands lsp
  :init
  (setq lsp-auto-guess-root nil)
  :config
  (setq lsp-prefer-flymake nil)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-header t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'top
              lsp-ui-doc-use-webkit t
              lsp-ui-doc-border (face-foreground 'default)

              lsp-ui-sideline-enable nil
              lsp-ui-sideline-ignore-duplicate t)
  :config
  (setq lsp-ui-sideline-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)))

(use-package company-posframe)
(use-package company-lsp
  :after company lsp-mode
  :init
  (setq company-lsp-cache-candidates 'auto)
  :commands company-lsp
  :config
  (if (display-graphic-p)
      (progn
        (require 'company-posframe)
        (company-posframe-mode 1)
        (require 'desktop) ;this line is needed.
        (push '(company-posframe-mode . nil)
              desktop-minor-mode-table)))
  (push 'company-lsp company-backends)
  :hook
  (after-init . global-company-mode))


;;; c/c++
;; using ccls: https://github.com/MaskRay/ccls
(use-package ccls
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
;; (use-package google-c-style
;;   :hook
;;   ((c-mode c++-mode) . google-set-c-style)
;;   (c-mode-common . google-make-newline-indent))
                                        ; (use-package cc-mode
                                        ;   :ensure nil
                                        ;   :config
                                        ;   (setq flycheck-clang-language-standard "c++11")
                                        ;   (global-set-key (kbd "C-c C-r") 'compile)
                                        ;   (global-set-key (kbd "C-c C-t") 'gdb)
                                        ;   (setq-default c-basic-offset 4)
                                        ;   (setq flycheck-clang-language-standard "gnu99"))

;; built-in support sh-mode
;; (use-package sh-mode)


(use-package rust-mode
  :init
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))))

(use-package lua-mode
  :mode
  ("\\.lua$" . lua-mode))

(use-package docker
  :commands docker-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "/usr/local/bin/pandoc")
  :config)
(use-package markdown-toc
  :defer t)

(use-package groovy-mode)
(use-package init-python
  :ensure nil)
(use-package init-go
  :ensure nil)
(use-package init-haskell
  :ensure nil)
(use-package init-typescript
  :ensure nil)
(use-package init-flutter
  :ensure nil)

(provide 'init-languages)
;;; init-languages.el ends here
