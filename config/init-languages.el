;;; package --- Summary
;; C++, C, Rust
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

;; (use-package tree-sitter
;;   :init
;;   (local-unset-key (kbd "M-t"))
;;   (use-package tree-sitter-langs)
;;   :config
;;   (defun tree-sitter-force-update()
;;     (interactive)
;;     (setq tree-sitter-tree nil)
;;     (tree-sitter--do-parse))
;;   (global-tree-sitter-mode)
;;   (add-hook 'prog-mode-hook #'tree-sitter-hl-mode)
;;   :bind
;;   ("M-t" . tree-sitter-force-update))


(use-package lsp-bridge
  :commands lsp-bridge-mode
  :load-path "/Users/zhangwei/code/z/lsp-bridge"
  :ensure nil
  :bind
  (:map lsp-bridge-mode-map
        ("M-." . lsp-bridge-find-def)
        ("M-n i" . lsp-bridge-find-impl)
        ("M-n RET" . lsp-bridge-code-action)
        ("M-n ." . lsp-bridge-find-def-other-window)
        ("M-," . lsp-bridge-find-def-return)
        ("M-n d" . lsp-bridge-lookup-documentation)
        ("M-n r" . lsp-bridge-rename)
        ("M-n n" . lsp-bridge-diagnostic-jump-next)
        ("M-n p" . lsp-bridge-diagnostic-jump-prev)
        ("M-n l" . lsp-bridge-diagnostic-list)
        ("M-n q" . lsp-bridge-restart-process))

  :init
  (use-package markdown-mode)
  (use-package posframe)

  :config
  (local-unset-key (kbd "M-,"))
  (local-unset-key (kbd "M-."))
  (setq lsp-bridge-enable-auto-format-code t)
  (setq lsp-bridge-auto-format-code-idle 3)

  (require 'cl-lib)
  (setq lsp-bridge-get-project-path-by-filepath
        (lambda (path)
          (cl-dolist (project
                      '("/Users/zhangwei/code/golang/src/gitlab.bj.sensetime.com/diamond/service-providers/redis/redis-operator"
                        "/Users/zhangwei/code/sensetime/sp/mysql/service-broker"))
            (if (string-prefix-p project path)
                (cl-return project)
              nil))))

  ;; (use-package format-all
  ;;   :config
  ;;   (add-hook 'prog-mode-hook 'format-all-mode)
  ;;   :bind
  ;;   (:map lsp-bridge-mode-map ;; no format-all-mode-map, use lsp bridge
  ;;         ("M-n f" . format-all-buffer)))

  (global-lsp-bridge-mode))

(use-package dash-at-point
  :load-path "plugin/dash-at-point"
  :ensure nil
  :init
  (global-unset-key (kbd "C-c d"))
  :bind
  (("C-c d" . dash-at-point)
   ("C-c e" . dash-at-point-with-docset)))

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
