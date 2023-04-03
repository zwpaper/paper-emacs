;;; package --- Summary
;; C++, C, Rust
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package copilot
  :load-path "~/code/copilot.el"
  :ensure nil
  :hook
  (prog-mode . copilot-mode)
  :bind
  (:map copilot-mode-map
        ("M-n e" . copilot-accept-completion-by-line)
        ("M-n w" . copilot-accept-completion-by-word)
        ("M-n a" . copilot-accept-completion)))

(use-package lsp-bridge
  :commands lsp-bridge-mode
  :load-path "~/code/el/lsp-bridge"
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

  :hook
  (prog-mode . lsp-bridge-mode)

  :config
  (local-unset-key (kbd "M-,"))
  (local-unset-key (kbd "M-."))
  (setq lsp-bridge-enable-auto-format-code t)
  (setq lsp-bridge-auto-format-code-idle 3)
  (setq markdown-enable-highlighting-syntax t)

  (require 'cl-lib)
  ;; (use-package format-all
  ;;   :config
  ;;   (add-hook 'prog-mode-hook 'format-all-mode)
  ;;   :bind
  ;;   (:map lsp-bridge-mode-map ;; no format-all-mode-map, use lsp bridge
  ;;         ("M-n f" . format-all-buffer)))
  )

(when (treesit-available-p)
  (require 'treesit)
  (global-set-key (kbd "C-M-a") 'treesit-beginning-of-defun)
  (global-set-key (kbd "C-M-e") 'treesit-end-of-defun)

  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (go-mode       . go-ts-mode)
          (go-mod-mode       . go-mod-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (json-mode         . json-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (rust-mode       . rust-ts-mode)
          (sh-mode         . bash-ts-mode)
          (yaml-mode         . yaml-ts-mode)))

  (defun kw-go-ts-mode-setup ())
                                        ;(setq-local treesit-defun-type-regexp "\\(method\\|function\\)_declaration"))
  (add-hook 'go-ts-mode-hook #'kw-go-ts-mode-setup)
  )


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
  ;; :config
  ;; (use-package reformatter
  ;;   :config
  ;;   (reformatter-define rust-formatter
  ;;     :program "cargo"
  ;;     :args '("fmt" "--" "--emit" "stdout")))
  )

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
