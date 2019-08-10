;;; package --- Summary
;; Bean Count init
;; A Double Entry Accounting System
;;; Commentary:

;;; Code:
(use-package beancount
  :load-path "~/.emacs.d/plugin"
  :ensure nil
  :mode
  ("\\.bean$" . beancount-mode))

(provide 'init-beancount)
;;; init-beancount.el ends here
