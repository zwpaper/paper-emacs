;;; package --- Summary
;; LSP init
;; Currently Nox
;;; Commentary:

;;; Code:

(use-package nox
  :ensure nil
  :quelpa (nox
           :upgrade t
           :fetcher github
           :repo "manateelazycat/nox")
  :init
  (defun go-mode-before-save-hook ()
    (when (eq major-mode 'go-mode)
      (nox-format-buffer)))
  :custom
  (add-hook 'before-save-hook #'go-mode-before-save-hook)
  :hook
  (go-mode . (lambda () (nox-ensure)))

  :bind
  (:map nox-mode-map
        ("M-n d" . nox-show-doc)
        ("M-n f" . nox-format-buffer)))


(provide 'init-lsp)
;;; init-go.el ends here
