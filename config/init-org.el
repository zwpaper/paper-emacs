;;; package --- Summary
;; org mode
;; org capture, org protocal
;;; Commentary:

;;; Code:

;;; org mode
(require 'init-const)

(use-package org
  :defer t
  :bind (:map org-mode-map
              ("C-M-f" . org-do-demote)
              ("C-M-b" . org-do-promote)
              ("C-M-p" . org-move-subtree-up)
              ("C-M-n" . org-move-subtree-down)
              ("C-M-<return>" . org-insert-todo-subheading)
              ("C-c i" . org-insert-image))
  :config
  (setq org-log-done t)
  ;; (setq org-agenda-files (list (concat org-path "/tasks")))
  (setq org-image-actual-width '(600))

  ;;; set org agenda prefix to project name
  (defun org-agenda-prefix-project ()
    (let ((x (nth 0 (org-get-outline-path))))
      (if x
          (concat "[" (org-format-outline-path (list x)) "]")
        "")))
  ;;; %b can do breadcrumb, but make it look mess
  ;; (setq org-agenda-prefix-format " %i %?-14(org-agenda-prefix-project) ")

  (defun org-insert-image ()
    "insert a image from clipboard"
    (interactive)
    (let* ((path (concat default-directory "images/"))
           (image-file (concat
                        path
                        (buffer-name)
                        (format-time-string "_%Y%m%d_%H%M%S.png"))))
      (if (not (file-exists-p path))
          (mkdir path))
      ;; (do-applescript (concat
      ;;                  "set the_path to \"" image-file "\" \n"
      ;;                  "set png_data to the clipboard as «class PNGf» \n"
      ;;                  "set the_file to open for access (POSIX file the_path as string) with write permission \n"
      ;;                  "write png_data to the_file \n"
      ;;                  "close access the_file"))

      (shell-command (concat "pngpaste " image-file))
      (org-insert-link nil
                       (concat "file:" image-file)
                       "")
      (message image-file))
    (org-display-inline-images))

  ;; Custom commands
  ;;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(("o" "At the office" tags-todo "#office"
           ((org-agenda-overriding-header "#office"))
           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))
          ("j" "Job, work tasks" tags-todo "+CATEGORY=\"Work\"+todo=\"TODO\""
           ((org-agenda-overriding-header "Work"))
           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))
          ("h" "At the home" tags-todo "#home"
           ((org-agenda-overriding-header "home")))
          ("p" "Priority" tags-todo "+PRIORITY=\"A\"")
          ("w" "Waitting" todo "WAITING")
          ("n" "Next" todo "NEXT")
          ("r" . "Review")
          ("ry" "Closed Yesterday"
           tags (concat "+TODO=\"DONE\""
                        "+CLOSED>=\""
                        (format-time-string "[%Y-%m-%d]" (time-subtract (current-time) (days-to-time 1)))
                        "\""))
          ("rw" "Get Current: Review Previous Week"
           ((agenda "" ((org-agenda-start-day (concat "-" (number-to-string (+ 13 (nth 6 (decode-time)))) "d"))
                        (org-agenda-span (+ 14 (nth 6 (decode-time))))
                        (org-agenda-repeating-timestamp-show-all t)
                        (org-agenda-entry-types '(:deadline :timestamp :sexp)) ; show due tasks, meetings
                        (org-agenda-show-log t)
                        (org-agenda-prefix-format "%-12t% s"))))
           )))

  :hook
  (org-mode . (lambda () (org-display-inline-images t)))
  (org-mode . (lambda () (add-hook 'before-save-hook 'org-redisplay-inline-images nil 'local))))


(defun zjournal-filename ()
  (org-path-concat "journal"
                   (concat (format-time-string "%Y-%m")
                           ".org")))
(defun zjournal-open ()
  (interactive)
  (switch-to-buffer (find-file-noselect (zjournal-filename))))

(defun zjournal-template ()
  (interactive)
  (switch-to-buffer (find-file-noselect "/Users/zhangwei/.emacs.d/snippets/org-mode/journal")))

(global-set-key (kbd "C-x j o") 'zjournal-open)
(global-set-key (kbd "C-x j t") 'zjournal-template)


(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(eval-after-load "org"
  (use-package ox-gfm
    :ensure t
    :config
    (setq org-src-fontify-natively t)))
;;;; Capture
(setq org-default-notes-file (concat org-path "/inbox.org"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
;; (setq org-refile-targets `((,(concat org-path "/tasks/personal.org") :maxlevel . 3)
;;                            (,(concat org-path "/tasks/work.org") :maxlevel . 9)
;;                            (,(concat org-path "/tasks/next.org") :maxlevel . 3)
;;                            (,(concat org-path "/tasks/family.org") :maxlevel . 3)
;;                            (,(concat org-path "/tasks/maybe.org") :level . 1)))
;;
(use-package org-protocol
  :ensure nil)
(use-package org-capture
  :ensure nil
  :init
  (defun get-journal-file-this-month ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m")))
      (expand-file-name (concat org-path "/" daily-name ".org"))))
  :config
  (defun get-journal-file-this-month ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m")))
      (expand-file-name (concat org-path "/" daily-name ".org"))))
;;;; Templates
  (setq org-capture-templates '(("b" "Book Reading Task" entry
                                 (file+headline (lambda () (org-path-concat "tasks" "readList.org"))
                                                "inbox")
                                 "* TODO %^{Book Name}\n%u\n%a\n" :clock-in t :clock-resume t)
                                ("w" "Work Task" entry
                                 (file+headline (lambda () (org-path-concat "tasks" "inbox.org"))
                                                "Work")
                                 "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t)
                                ("t" "Personal Task" entry
                                 (file+headline (lambda () (org-path-concat "tasks" "inbox.org"))
                                                "Personal")
                                 "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t)
                                ("n" "Notes" entry
                                 (file+headline (lambda () (org-path-concat "tasks" "inbox.org"))
                                                "Notes")
                                 "* %^{heading} %t %^g\n  %?\n")
                                ("j" "Journal Note" entry
                                 (file+olp+datetree
                                  (lambda () (zjournal-filename)))
                                 "* %T %?\n  %i\n  From: %a" :empty-lines 1 :prepend t)
;;;; org protocol capture from outside Emacs
;;;; Should be update later
                                ("l" "Notes with link" plain
                                 (file+function (lambda () (org-path-concat "notes" "inbox.org"))
                                                org-capture-template-goto-link)
                                 "  %U - %?\n\n  %:initial" :empty-lines 1)))
  (defun org-capture-template-goto-link ()
    (org-capture-put :target (list 'file+headline
                                   (nth 1 (org-capture-get :target))
                                   (org-capture-get :annotation)))
    (org-capture-put-target-region-and-position)
    (widen)
    (let ((hd (nth 2 (org-capture-get :target))))
      (goto-char (point-min))
      (if (re-search-forward
           (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
          (org-end-of-subtree)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " hd "\n"))))

  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "remember") (width . 80) (height . 16)
                  (top . 400) (left . 300)
                  (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                  ))
    (select-frame-by-name "remember")
    (org-capture))
;;;; End capture from outside Emacs
  (global-set-key (kbd "C-c c") 'org-capture))


;;; GTD
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
                          (sequence "PROJECT" "TARGET" "MAYBE" "|")))
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "red" :weight bold))
        ("PROJECT" . (:foreground "green" :weight bold))
        ("TARGET" . (:foreground "purple" :weight bold))
        ("MAYBE" . (:foreground "gray"))
        ("HOLD" . (:foreground "gray"))
        ("WAITING" . "magenta")))
(setq org-tag-alist (quote (("#errand" . ?e)
                            ("#office" . ?o)
                            ("#home" . ?h)
                            ("#computer" . ?c)
                            (:newline)
                            ("HOLD" . ?H))))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; (use-package org-download
;;   :after org
;;   :config
;;   (setq-default org-download-image-dir "./images")
;;   (setq-default org-download-heading-lvl nil)
;;   (setq org-startup-with-inline-images t)
;;   (setq org-download-screenshot-method "screencapture -i %s")
;;   :hook
;;   (org-mode . org-download))

;;; hugo blog
(use-package ox-hugo
  :after ox)

(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (shell . t)))

(use-package deft
  :commands (deft)
  :config
  (setq deft-extensions '("md" "markdown" "tex" "org"))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  :bind
  (("M-s M-o" . deft)))

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ"))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(provide 'init-org)
;;; init-org.el ends here
