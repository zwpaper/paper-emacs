;;; package --- Summary
;; Hydra init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package hydra
  :config
  (defhydra hydra-move-window (global-map "M-o")
    "
   ^^^^Scroll
  -^^^^-------------^--------------------
     ^_k_^    up    ^_i_^    increase test
     ^_j_^    down  ^_d_^    decrease test
  -^^^^-------------^--------------------
"
    ("i" text-scale-increase)
    ("d" text-scale-decrease)
    ("j" scroll-up-line)
    ("k" scroll-down-line))
  )


(use-package pretty-hydra
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(provide 'init-hydra)
;;; init-hydra.el ends here
