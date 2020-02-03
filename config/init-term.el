;;; package --- Summary
;; init vterm
;;; Commentary:

;;; Code:

(use-package vterm
  :bind
  ("M-m n" . vterm)
  ("M-m o" . vterm-other-window)
  ("M-m r" . rename-buffer))

;; aweshell
(use-package aweshell
  :ensure nil
  :quelpa (aweshell
           :fetcher github
           :repo "manateelazycat/aweshell"
           :files ("aweshell.el"))
  :bind
  ("M-m e" . aweshell-new)
  ("M-m m" . aweshell-dedicated-toggle)
  :init
  (use-package eshell-up)
  (use-package eshell-did-you-mean)
  (use-package eshell-prompt-extras)
  :config
  (setq eshell-up-ignore-case nil))


(provide 'init-term)
;;; init-vterm.el ends here
