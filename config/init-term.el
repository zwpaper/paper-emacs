;;; package --- Summary
;; init vterm
;;; Commentary:

;;; Code:

(global-unset-key (kbd "M-m"))
(use-package vterm
  :config
  (use-package posframe-project-term
    :ensure nil
    :load-path "/Users/zhangwei/code/z/posframe-project-term")
  :bind
  (("M-m n" . vterm)
   ("M-m o" . posframe-project-term-toggle)
   ("M-m r" . rename-buffer)
   :map vterm-mode-map
   ("C-c C-c" . vterm-send-C-c)
   ("M-m" . ())))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(provide 'init-term)
;;; init-vterm.el ends here
