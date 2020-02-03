;;; package --- Summary
;; init Python
;;; Commentary:

;;; Code:

(use-package python-mode
  :hook
  (python-mode . lsp)
  :init
  :config)

(use-package lsp-python-ms
  :ensure t
  :init
  (setq lsp-python-ms-executable
        "~/.bin/Microsoft.Python.LanguageServer")
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(provide 'init-python)
;;; init-python.el ends here
