;;; package --- Summary
;; C++, C, Go, Python, Rust, Haskell
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

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
  :commands lsp
  :init
  (setq lsp-auto-guess-root nil)
  :config
  (setq lsp-prefer-flymake nil)
  :hook
  (before-save . lsp-format-buffer)
  (prog-mode . lsp))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after company lsp-mode
  :init
  (setq company-lsp-cache-candidates 'auto)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  :hook
  (after-init . global-company-mode))

;; c/c++
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

(use-package go-mode)

(use-package rust-mode
  :init
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))))

(use-package python-mode
  :init
  :config)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks"))))

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

(provide 'init-languages)
;;; init-languages.el ends here
