;;; package --- Summary
;; org mode
;; org capture, org protocal
;;; Commentary:

;;; Code:

;;; org mode
(require 'init-const)

(use-package org
  :ensure org-plus-contrib
  :bind (("C-M-f" . org-do-demote)
         ("C-M-b" . org-do-promote)
         ("C-M-p" . org-move-subtree-up)
         ("C-M-n" . org-move-subtree-down))
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list (concat org-path "/tasks"))))

  ;; Custom commands
  (setq org-agenda-custom-commands
        '(("o" "At the office" tags-todo "#office"
           ((org-agenda-overriding-header "#office")))
          ("h" "At the home" tags-todo "#home"
           ((org-agenda-overriding-header "home")))
          ("p" "Priority" tags-todo "+PRIORITY=\"A\"")
          ("w" "Waitting" todo "WAITING")
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
           ))))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(eval-after-load "org"
  (use-package ox-gfm
    :ensure t
    :config
    (setq org-src-fontify-natively t)))
;;;; Capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-path "/inbox.org"))
(setq org-refile-targets '(((concat org-path "/tasks/personal.org") :maxlevel . 3)
                           ((concat org-path "/tasks/work.org") :maxlevel . 3)
                           ((concat org-path "/tasks/next.org") :maxlevel . 3)
                           ((concat org-path "/tasks/family.org") :maxlevel . 3)
                           ((concat org-path "/tasks/maybe.org") :level . 1)))

(use-package org-protocol
  :ensure nil)
(use-package org-capture
  :ensure nil)

;;;; Tamplates
(add-to-list 'org-capture-templates
             '("b" "Book Reading Task" entry
               (file+headline (concat org-path "/tasks/readList.org") "inbox")
               "* TODO %^{Book Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("w" "Work Task" entry
               (file+headline (concat org-path "/tasks/inbox.org") "Work")
               "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("t" "Personal Task" entry
               (file+headline (concat org-path "/tasks/inbox.org") "Personal")
               "* TODO %^{Task Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("n" "Notes" entry
               (file+headline (concat org-path "/tasks/inbox.org"), "Notes")
               "* %^{heading} %t %^g\n  %?\n"))

;;;; org protocol capture from outside Emacs
;;;; Should be update later
(add-to-list 'org-capture-templates
             '("l" "Notes with link" plain
               (file+function (concat org-path "/notes/inbox.org") org-capture-template-goto-link)
               "  %U - %?\n\n  %:initial" :empty-lines 1))
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

;;; GTD
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "red" :weight bold))
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

(use-package org-download
  :after org
  :config
  (setq org-startup-with-inline-images t)
  (setq org-download-screenshot-method "screencapture -i %s"))

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

(provide 'init-org)
;;; init-org.el ends here
