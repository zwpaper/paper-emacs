;;; package --- Summary
;; C++, C, Go, Python
;;; Commentary:

;;; Code:
(use-package flycheck
  :hook
  (go-mode rust-mode python-mode))

(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'before-save-hook 'lsp-format-buffer)
  :config
;  (setq lsp-prefer-flymake nil)
  :hook
  (go-mode rust-mode python-mode))
(use-package lsp-ui
  :commands lsp-ui
  :hook
  (lsp-mode))
(use-package company-lsp
  :after company lsp-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  :hook
  (after-init . global-company-mode))

;; Bash
(add-hook 'sh-mode-hook #'lsp-sh-enable)

;; C++
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;; C
(add-hook 'c-mode-hook (lambda () (setq flycheck-clang-language-standard "gnu99")))
(setq-default c-basic-offset 4)
(global-set-key (kbd "C-c C-r") 'compile)
(global-set-key (kbd "C-c C-t") 'gdb)

;;; Go
; (setq gofmt-command "goimports")
; (defun my-go-mode-hook ()
;   "Auto format and import on save."
; ;  (add-hook 'before-save-hook 'gofmt-before-save)
;   (add-hook 'before-save-hook 'lsp-format-buffer)
;   (if (not (string-match "go" compile-command))
;       (set (make-local-variable 'compile-command)
;            "go generate && go build -v && go test -v && go vet"))
;   (local-set-key (kbd "M-.") 'godef-jump))
;
;(add-hook 'go-mode-hook #'yas-minor-mode)
; (add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20160715.1705")
; (add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949/")
; (add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711/")
; (require 'go-mode-load)
; (require 'go-autocomplete)
; (require 'auto-complete-config)
; (add-hook 'go-mode-hook 'go-eldoc-setup)
; (require 'golint)
(use-package go-mode
  :init
  :config
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet")))
;  (add-hook 'go-mode-hook 'lsp)
;  (add-hook 'go-mode-hook 'flycheck-mode)
;  :hook
;  (go-mode-hook . lsp)
;  (go-mode-hook . flycheck-mode))

;;; Rust
(use-package rust-mode
  :init
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))))

;; ;; python
;(use-package lsp-python
;  :config
;  (lsp-define-stdio-client lsp-python "python"
;                           (lsp-make-traverser #'(lambda (dir)
;                                                   (directory-files
;                                                    dir
;                                                    nil
;                                                    "\\(__init__\\|setup\\)\\.py\\|Pipfile")))
;                           '("pyls"))
;
;  )
;; (add-hook 'python-mode-hook #'lsp-python-enable)

(use-package python-mode
  :init
  :config
  (add-hook 'python-mode-hook  'lsp))


;;   (setq   python-shell-interpreter "ipython"
;;           python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt -i"
;;           python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;           python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;           python-shell-completion-setup-code
;;           "from IPython.core.completerlib import module_completion"
;;           python-shell-completion-module-string-code
;;           "';'.join(module_completion('''%s'''))\n"
;;           python-shell-completion-string-code
;;           "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
;;
;; (require 'ansi-color)
;; (define-coding-system-alias 'UTF-8 'utf-8)
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;; Markdown
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
  :defer t
  )

(provide 'init-languages)
;;; init-languages.el ends here
