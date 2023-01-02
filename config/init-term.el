;;; package --- Summary
;; init vterm
;;; Commentary:

;;; Code:

(global-unset-key (kbd "M-m"))
(use-package vterm
  :bind
  (("M-m n" . vterm)
   ("M-m o" . vterm-other-window)
   ("M-m r" . rename-buffer)
   :map vterm-mode-map
   ("M-m" . ())))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(provide 'init-term)
;;; init-vterm.el ends here
