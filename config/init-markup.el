;; init-markup.el --- Initialize Markup Language configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 zwPapEr

;; Author: zwPapEr <zw.paper@gmail.com>
;; URL: https://github.com/zwpaper/.emacs.d

;; This file is not part of GNU Emacs.
;;
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;

;;; Commentary:
;;
;; Markup configurations.
;;

;;; Code:

(use-package graphql-mode)

(use-package json-mode
  :custom
  (js-indent-level 2))

(provide 'init-markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
