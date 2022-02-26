;;; package --- Summary
;; LSP init
;; Currently Nox
;;; Commentary:

;;; Code:
(use-package company-posframe)
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :init
  (global-unset-key (kbd "M-n"))
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-keymap-prefix "M-n"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  (defun should-format-modes-before-save-hook ()
    (when (or (eq major-mode 'go-mode)
              (eq major-mode 'rust-mode))
      (lsp-format-buffer)
      (lsp-organize-imports)))

  (use-package which-key
    :config
    (which-key-mode))
  :hook ((go-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (before-save . should-format-modes-before-save-hook))
  :config
  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol
    :after lsp-mode)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

  (setq lsp-auto-guess-root nil)

  (use-package lsp-ui
    :custom-face
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :pretty-hydra
    ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
             :color amaranth :quit-key "q")
     ("Doc"
      (("d e" (progn
                (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
        "enable" :toggle lsp-ui-doc-mode)
       ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
        "signature" :toggle lsp-ui-doc-include-signature)
       ("d t" (setq lsp-ui-doc-position 'top)
        "top" :toggle (eq lsp-ui-doc-position 'top))
       ("d b" (setq lsp-ui-doc-position 'bottom)
        "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
       ("d p" (setq lsp-ui-doc-position 'at-point)
        "at point" :toggle (eq lsp-ui-doc-position 'at-point))
       ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
        "header" :toggle lsp-ui-doc-header)
       ("d f" (setq lsp-ui-doc-alignment 'frame)
        "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
       ("d w" (setq lsp-ui-doc-alignment 'window)
        "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
      "Sideline"
      (("s e" (progn
                (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
        "enable" :toggle lsp-ui-sideline-mode)
       ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
        "hover" :toggle lsp-ui-sideline-show-hover)
       ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
        "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
       ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
        "symbol" :toggle lsp-ui-sideline-show-symbol)
       ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
        "code actions" :toggle lsp-ui-sideline-show-code-actions)
       ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
        "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
      "Action"
      (("h" backward-char "←")
       ("j" next-line "↓")
       ("k" previous-line "↑")
       ("l" forward-char "→")
       ("C-a" mwim-beginning-of-code-or-line nil)
       ("C-e" mwim-end-of-code-or-line nil)
       ("C-b" backward-char nil)
       ("C-n" next-line nil)
       ("C-p" previous-line nil)
       ("C-f" forward-char nil)
       ("M-b" backward-word nil)
       ("M-f" forward-word nil)
       ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
    :hook
    (lsp-mode . lsp-ui-mode)
    (shell-script-mode . lsp-mode)
    :bind (("M-n u" . lsp-ui-imenu)
           :map lsp-ui-mode-map
           ("M-n k" . lsp-ui-hydra/body)
           ("M-RET" . lsp-ui-sideline-apply-code-actions))
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.2
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'top
                lsp-ui-doc-border (face-foreground 'default)

                lsp-ui-sideline-enable t
                lsp-ui-sideline-show-hover nil
                lsp-ui-sideline-show-diagnostics nil
                lsp-ui-sideline-show-code-actions t
                lsp-ui-sideline-ignore-duplicate t

                lsp-ui-imenu-enable t
                lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                      ,(face-foreground 'font-lock-string-face)
                                      ,(face-foreground 'font-lock-constant-face)
                                      ,(face-foreground 'font-lock-variable-name-face)))
    :config
    (use-package lsp-grammarly
      :ensure t
      :hook (text-mode . (lambda ()
                           (require 'lsp-grammarly)
                           (lsp))))  ; or lsp-deferred

    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    ;; Reset `lsp-ui-doc-background' after loading theme
    (add-hook 'after-load-theme-hook
              (lambda ()
                (setq lsp-ui-doc-border (face-foreground 'default))
                (set-face-background 'lsp-ui-doc-background
                                     (face-background 'tooltip))))))

(use-package devdocs-browser)

(provide 'init-lsp)
;;; init-lsp.el ends here
