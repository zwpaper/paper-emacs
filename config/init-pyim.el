;;; package --- Summary
;; init Pyim
;;; Commentary:

;;; Code:

(setenv "RIME_PATH" (expand-file-name "~/code/repo/librime"))

(use-package liberime-config
  :ensure nil
  :quelpa (liberime-config
           :fetcher github
           :repo "merrickluo/liberime"
           :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el"))
  :init
  (add-hook 'liberime-after-start-hook
            (lambda ()
              (liberime-select-schema "luna_pinyin_simp"))))

(use-package pyim
  :after liberime-config
  :demand t
  :config
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'rime)
  ;; (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
  ;; (expand-file-name "~/.emacs.d/pyim/rime/"))
  ;; (expand-file-name "~/Library/Rime")
                                        ; (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (expand-file-name "~/.emacs.d/pyim/rime/"))
                                        ; (liberime-select-schema "luna_pinyin_simp")
  (setq pyim-page-length 9)
  (define-key pyim-mode-map "]" 'pyim-page-next-page)
  (define-key pyim-mode-map "[" 'pyim-page-previous-page)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中 英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 使用 pupup-el 来绘制选词框
  (if window-system
      (progn
        (use-package posframe)
        (setq pyim-page-tooltip 'posframe))
    (setq pyim-page-tooltip 'popup))

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'init-pyim)
;;; init-pyim.el ends here
