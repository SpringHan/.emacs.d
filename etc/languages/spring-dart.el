;;; This file is used to config the dart.

;;; Highlighting
(gpack dart-mode)

(gpack lsp-dart
  :hook (dart-mode-hook . (lambda () (require 'lsp-dart) (lsp-deferred))))

(provide 'spring-dart)
