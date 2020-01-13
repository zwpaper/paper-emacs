;;; package --- Summary
;; init vterm
;;; Commentary:

;;; Code:

(use-package vterm
  :bind
  ("M-n n" . vterm)
  ("M-n o" . vterm-other-window)
  ("M-n r" . rename-buffer))

(provide 'init-vterm)
;;; init-vterm.el ends here
