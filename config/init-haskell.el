;;; package --- Summary
;; Haskell init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (require 'lsp-haskell)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks"))))

(use-package lsp-haskell
  :config
  ;; may need only for macOS
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  :hook
  (haskell-mode . lsp))


(provide 'init-haskell)
;;; init-haskell.el ends here
