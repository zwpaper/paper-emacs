;;; package --- Summary
;; Hydra init
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package hydra)

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

(provide 'init-hydra)
;;; init-hydra.el ends here
