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

  (add-hook 'before-save-hook 'format-all-buffer)

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

  (setq compilation-read-command nil)

  (defun project-compile ()
    "Run compile over the current project or pwd."
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 (symbol-value 'default-directory))))
      (compile "make build")))

  (defun project-image ()
    "Run compile over the current project or pwd."
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 (symbol-value 'default-directory)))
          (compilation-scroll-output t))
      (compile "make image")))

  (defun project-image-push ()
    "Run compile over the current project or pwd."
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 (symbol-value 'default-directory))))
      (compile "make push")))

  :bind
  (:map go-mode-map
        ("C-c C-r" . project-compile)
        ("C-c C-i" . project-image)
        ("C-c C-p" . project-image-push)
        ("C-c R" . go-remove-unused-imports))
  :hook
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
