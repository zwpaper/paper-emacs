;;; package --- Summary
;; C++, C, Go, Python
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

;; YASnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
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
            :config (add-to-list 'flycheck-posframe-inhibit-functions
                                 #'(lambda () (bound-and-true-p company-backend))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  :config
  (setq lsp-prefer-flymake nil)
  :hook
  (before-save . lsp-format-buffer)
  (prog-mode . lsp))

(use-package lsp-ui
  :init
  (setq lsp-ui-flycheck-enable t)
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

;; Bash
; (add-hook 'sh-mode-hook #'lsp-sh-enable)

;; c/c++
; (use-package cc-mode
;   :ensure nil
;   :config
;   (setq flycheck-clang-language-standard "c++11")
;   (global-set-key (kbd "C-c C-r") 'compile)
;   (global-set-key (kbd "C-c C-t") 'gdb)
;   (setq-default c-basic-offset 4)
;   (setq flycheck-clang-language-standard "gnu99"))

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
;(add-hook 'go-mode-hook #')
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
           "go generate && go build -v && go test -v && go vet"))
  :hook
  (yas-minor-mode))

;;; Rust
(use-package rust-mode
  :init
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))
  :hook
  (rust-mode . flycheck-mode)
  (rust-mode . lsp))

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
  :config)


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
