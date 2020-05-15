;;; package --- Summary
;; init Pyim
;;; Commentary:

;;; Code:

(use-package rime
  :ensure t
  :init
  (setq rime-user-data-dir "~/.emacs.d/rime/")
  (setq rime-share-data-dir "~/Dropbox/AppSync/Rime")
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p))

  (defun +rime-force-enable ()
    "强制 `rime' 使用中文输入状态。
  如果当前不是 `rime' 输入法，则先激活 `rime' 输入法。如果当前是
  `evil' 的非编辑状态，则转为 `evil-insert-state'。"
    (interactive)
    (let ((input-method "rime"))
      (unless (string= current-input-method input-method)
        (activate-input-method input-method))
      (when (rime-predicate-evil-mode-p)
        (if (= (+ 1 (point)) (line-end-position))
            (evil-append 1)
          (evil-insert 1)))
      (rime-force-enable)))

  (defun +rime-convert-string-at-point (&optional return-cregexp)
    "将光标前的字符串转换为中文."
    (interactive "P")
    (+rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (line-beginning-position) (point))))
          code
          length)
      (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
             (setq code (replace-regexp-in-string
                         "^[-']" ""
                         (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
                 (delete-region (region-beginning) (region-end))
               (when (> length 0)
                 (delete-char (- 0 length))))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))

  :custom
  (rime-librime-root "~/.emacs.d/plugin/librime/dist")
  :bind
  ("M-j" . #'+rime-convert-string-at-point)
  (:map rime-mode-map
        ("M-j" . #'rime-inline-ascii)
        ("C-`" . rime-send-keybinding)))


;; (setenv "RIME_PATH" (expand-file-name "~/code/repo/librime"))
;;
;; (use-package liberime-config
;;   :ensure nil
;;   :quelpa (liberime-config
;;            :fetcher github
;;            :repo "merrickluo/liberime"
;;            :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el"))
;;   :init
;;   (add-hook 'liberime-after-start-hook
;;             (lambda ()
;;               (liberime-select-schema "double_pinyin_flypy"))))
;;
;; (use-package pyim
;;   :after liberime-config
;;   :demand t
;;   :config
;;   (setq default-input-method "pyim")
;;   (setq pyim-default-scheme 'rime)
;;   ;; (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
;;   ;; (expand-file-name "~/.emacs.d/pyim/rime/"))
;;   ;; (expand-file-name "~/Library/Rime")
;;                                         ; (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (expand-file-name "~/.emacs.d/pyim/rime/"))
;;                                         ; (liberime-select-schema "luna_pinyin_simp")
;;   (setq pyim-page-length 9)
;;   (define-key pyim-mode-map "]" 'pyim-page-next-page)
;;   (define-key pyim-mode-map "[" 'pyim-page-previous-page)
;;
;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中 英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))
;;
;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))
;;
;;   ;; 使用 pupup-el 来绘制选词框
;;   (if window-system
;;       (progn
;;         (use-package posframe)
;;         (setq pyim-page-tooltip 'posframe))
;;     (setq pyim-page-tooltip 'popup))
;;
;;   ;; 让 Emacs 启动时自动加载 pyim 词库
;;   (add-hook 'emacs-startup-hook
;;             #'(lambda () (pyim-restart-1 t)))
;;   :bind
;;   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;    ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'init-pyim)
;;; init-pyim.el ends here
