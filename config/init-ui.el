;;; package --- Summary
;; init UI
;;; Commentary:

    (use-package all-the-icons
      :ensure nil
      :load-path "~/code/elisp/all-the-icons.el")
    ;; (all-the-icons-install-fonts))
    (use-package nerd-icons
      :load-path "~/.emacs.d/plugin/nerd-icons"
      :ensure nil)
    (use-package all-the-icons-ivy-rich
      :ensure t
      :init (all-the-icons-ivy-rich-mode 1))

    (use-package ivy-rich
      :ensure t
      :init (ivy-rich-mode 1)
      :config
      (setq all-the-icons-ivy-rich-icon-size 0.8)
      (defun ivy-rich--switch-buffer-directory! (orig-fun &rest args)
        (cl-letf (((symbol-function 'directory-file-name) #'file-name-directory))
          (apply orig-fun args)))
      (advice-add 'ivy-rich--switch-buffer-directory
                  :around #'ivy-rich--switch-buffer-directory!))

      (use-package all-the-icons-ibuffer
        :ensure t
        :init (all-the-icons-ibuffer-mode 1))

      (use-package ibuffer-projectile
        :config
        (add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)))))

      ;; dired auto use the other window path as target
      (setq dired-dwim-target t)

      (cond ((eq system-type 'windows-nt)
         ;; Windows-specific code goes here.
         )
        ((eq system-type 'gnu/linux)
         ;; Linux-specific code goes here.
         )
        ((eq system-type 'darwin)
         ;; macOS code goes here.
         ))

      (if (eq system-type 'darwin)
      (progn
        ;; Set default font
        ;;; 如果配置好，这24个汉字与下面个48英文字母应该等长
        ;;; here are 24 chinese and 48 english chars, ended.
        (set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 140
                    :weight 'normal
                    :width 'normal)
        (set-fontset-font t 'han      (font-spec
                       :family "PingFang SC"
                       :size 16
                       ))
        (set-fontset-font t 'cjk-misc (font-spec
                       :family "PingFang SC"
                       :size 16
                       ))))

(provide 'init-ui)
;;; init-ui.el ends here
