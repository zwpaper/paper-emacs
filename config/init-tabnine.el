;;; package --- Summary
;; TabNine init
;;; Commentary:

;;; Code:

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0))

(defun use-package-list-insert (elem xs &optional anchor after test)
  "Insert ELEM into the list XS.
  If ANCHOR is also a keyword, place the new KEYWORD before that
  one.
  If AFTER is non-nil, insert KEYWORD either at the end of the
  keywords list, or after the ANCHOR if one has been provided.
  If TEST is non-nil, it is the test used to compare ELEM to list
  elements. The default is `eq'.
  The modified list is returned. The original list is not modified."
  (let (result)
    (dolist (k xs)
      (if (funcall (or test #'eq) k anchor)
          (if after
              (setq result (cons k result)
                    result (cons elem result))
            (setq result (cons elem result)
                  result (cons k result)))
        (setq result (cons k result))))
    (if anchor
        (nreverse result)
      (if after
          (nreverse (cons elem result))
        (cons elem (nreverse result))))))

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

(provide 'init-tabnine)
;;; init-tabnine.el ends here
