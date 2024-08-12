(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq-default tab-width 4)
(global-display-line-numbers-mode 1)
(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))

(setq-default buffer-file-coding-system 'utf-8-unix)
;; Use UTF-8 as much as possible with unix line endings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)


;; Move backup file to tmp folder
(setq  backup-directory-alist `((".*" . ,temporary-file-directory))
       auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
       backup-by-copying t
       delete-old-versions t
       kept-new-versions 6
       kept-old-versions 2
       version-control t)


(global-display-fill-column-indicator-mode)
(setopt display-fill-column-indicator-column 80)

(setq electric-indent-mode nil)

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

;; Ace-window
(global-set-key (kbd "C-M-o") 'ace-window)     ; Ace-window
(setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
;; avy
(use-package avy
  :bind
  (("C-M-[" . avy-goto-char-2)))


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
