;;; package --- Summary
;; Golang init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package go-mode
  :init
  (add-to-list 'exec-path "~/code/go/bin")
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Format with `goimports' if possible, otherwise using `gofmt'
  ;; (when (executable-find "goimports")
  ;;   (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook 'nox-format-buffer)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports)

  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/cmd/goimports"
                      "golang.org/x/tools/cmd/gorename"

                      ;; "github.com/rogpeppe/godef"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct"
                      "github.com/golangci/golangci-lint/cmd/golangci-lint")
    "All necessary go tools.")

  ;; Do not use the -u flag for gopls, as it will update the dependencies to incompatible versions
  ;; https://github.com/golang/tools/blob/master/gopls/doc/user.md#installation
  (defvar go--tools-no-update '("golang.org/x/tools/gopls@latest")
    "All necessary go tools without update the dependencies.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (let ((proc-name "go-tools")
          (proc-buffer "*Go Tools*"))
      (dolist (pkg go--tools-no-update)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))

      (dolist (pkg go--tools)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))

  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golangci-lint run")
  (setq compilation-read-command nil)

  :bind
  (:map go-mode-map
        ("C-c C-r" . compile)
        ([remap xref-find-definitions] . godef-jump)
        ("C-c R" . go-remove-unused-imports))
  ;;:hook
  ;; (go-mode . (lambda() (run-with-idle-timer 10 nil 'maple/go-packages)))
  )

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
