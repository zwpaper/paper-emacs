;;; package --- Summary
;; init vterm
;;; Commentary:

;;; Code:

(use-package vterm
  :bind
  ("M-n n" . vterm)
  ("M-n o" . vterm-other-window)
  ("M-n r" . rename-buffer))

;; aweshell
(use-package aweshell
  :ensure nil
  :quelpa (aweshell
           :fetcher github
           :repo "manateelazycat/aweshell"
           :files ("aweshell.el"))
  :bind
  ("M-n e" . aweshell-new)
  :init
  (use-package eshell-up)
  (use-package eshell-did-you-mean)
  (use-package eshell-prompt-extras)
  :config
  (setq eshell-up-ignore-case nil))


(provide 'init-term)
;;; init-vterm.el ends here
