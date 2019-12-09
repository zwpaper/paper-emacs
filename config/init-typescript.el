;;; package --- Summary
;; TypeScript init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . ".*\\.tsx?"))))

;;; Using built-in support in lsp-mode
;;; yarn global add typescript-language-server
;;; yarn global add typescript

(provide 'init-typescript)
;;; init-typescript.el ends here
