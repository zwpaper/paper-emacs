;;; package --- Summary
;;; This is zwPapEr`s personal term.el file for Emacs
;;; Commentary:

;;; Code:

;; Move backup file to tmp folder
(setq  backup-directory-alist `((".*" . ,temporary-file-directory))
	   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	   backup-by-copying t
	   delete-old-versions t
	   kept-new-versions 6
	   kept-old-versions 2
	   version-control t)

(blink-cursor-mode -1)
(global-display-line-numbers-mode 1)
(global-display-fill-column-indicator-mode)
(setq electric-indent-mode nil)
(setopt display-fill-column-indicator-column 80)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq-default tab-width 4)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/plugin")
(add-to-list 'load-path "~/.emacs.d/config")

(setq-default buffer-file-coding-system 'utf-8-unix)
;; Use UTF-8 as much as possible with unix line endings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)


(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))

;;; Themes
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  ;; (load-theme 'modus-vivendi t)
  ;; fix terminal not load theme correctly
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (if (window-system)
                  (load-theme 'doom-nord t))
              (with-selected-frame frame (load-theme 'doom-nord t))))

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq current-theme 'doom-nord)
  ; (setq current-theme 'modus-vivendi)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package init-ui
  :ensure nil)

;;; Commands

;; Ace-window
(global-set-key (kbd "C-M-o") 'ace-window)     ; Ace-window
(setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
;; avy
(use-package avy
  :bind
  (("C-M-[" . avy-goto-char-2)))

(setq inhibit-compacting-font-caches t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-selectable-prompt t)
  (use-package ivy-rich)
  (use-package swiper)
  (use-package counsel
    :bind
    ("M-y" . counsel-yank-pop)
    ("M-x" . counsel-M-x)
    ("C-c g" . counsel-git)
    ("C-c j" . counsel-git-grep)
    ("C-c k" . counsel-rg)
    ("C-x C-f" . counsel-find-file)
    ("C-x r m" . counsel-bookmark))
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-rg)
   ("C-x C-f" . counsel-find-file)

   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish
  :hook
  ((prog-mode outline-mode conf-mode) . whitespace-mode)
  (markdown-mode . whitespace-mode)
  (yaml-mode . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'go-ts-mode)
(add-to-list 'aggressive-indent-excluded-modes 'rust-ts-mode))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; Always have a new line at the end of a file
(setq require-final-newline t)

;; Use iBuffer instead of Buffer List
(use-package ibuffer-projectile
  :ensure nil
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


;;; Promgram Modes
(use-package fish-mode)


(provide 'term)
;;; term.el ends here

