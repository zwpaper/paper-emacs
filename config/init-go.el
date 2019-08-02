;;; package --- Summary
;; Golang init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package go-mode
  :config
  (exec-path-from-shell-copy-env "GO111MODULE")
  (exec-path-from-shell-copy-env "GOPROXY")
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  (defun go-mode-before-save-hook ()
    (when (eq major-mode 'go-mode)
      (lsp-format-buffer)))

  (defvar maple/go-packages-list nil)

  (defun maple/go-packages()
    (or maple/go-packages-list
        (setq maple/go-packages-list (go-packages-native))))

  (setq go-packages-function 'maple/go-packages)

  :bind
  (:map go-mode-map
        ("C-c C-r" . compile)
        ([remap xref-find-definitions] . godef-jump)
        ("C-c R" . go-remove-unused-imports))
  :hook
  ;; (go-mode . (lambda() (run-with-idle-timer 10 nil 'maple/go-packages)))
  ((before-save . go-mode-before-save-hook)))

(use-package go-fill-struct
  ;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
  )
(use-package go-impl
  ;; go get -u github.com/josharian/impl
  ;; go get -u golang.org/x/tools/cmd/godoc
  )
(use-package go-tag
  ;; go get github.com/fatih/gomodifytags
  :bind (:map go-mode-map
              ("C-c t" . go-tag-add)
              ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(provide 'init-go)
;;; init-go.el ends here
