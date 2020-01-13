;;; package --- Summary
;; init Flutter
;;; Commentary:

;;; Code:

(use-package dart-mode
  :ensure-system-package (dart_language_server . "pub global activate dart_language_server")
  :custom
  (dart-format-on-save t)
  (dart-sdk-path (file-truename "~/repo/flutter/bin/cache/dart-sdk/")))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path (file-truename "~/repo/flutter")))

;; Optional
(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(provide 'init-flutter)
;;; init-flutter.el ends here
