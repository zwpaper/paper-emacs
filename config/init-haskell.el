;;; package --- Summary
;; Haskell init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks"))))

(provide 'init-haskell)
;;; init-haskell.el ends here
