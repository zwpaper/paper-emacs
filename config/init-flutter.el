;;; package --- Summary
;; init Flutter
;;; Commentary:

;;; Code:

(use-package dart-mode
  :hook
  (dart-mode . lsp)

  ;; :ensure-system-package (dart_language_server . "pub global activate dart_language_server")
  :custom
  (dart-format-on-save t)
  (dart-sdk-path (file-truename "~/repo/flutter/bin/cache/dart-sdk/"))
  (lsp-dart-sdk-path (file-truename "~/repo/flutter/bin/cache/dart-sdk/"))
  (lsp-dart-sdk-dir (file-truename "~/repo/flutter/bin/cache/dart-sdk/"))
  :hook
  (dart-mode . (lambda () (awesome-pair-mode 1))))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :config
  (exec-path-from-shell-copy-env "PUB_HOSTED_URL")
  (exec-path-from-shell-copy-env "FLUTTER_STORAGE_BASE_URL")
  :custom
  (flutter-sdk-path (file-truename "~/repo/flutter/")))

;; Optional
(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(provide 'init-flutter)
;;; init-flutter.el ends here
