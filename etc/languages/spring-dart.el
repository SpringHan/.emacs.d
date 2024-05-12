;;; This file is used to config the dart.

;;; Highlighting
(gpack dart-mode
  :hook (dart-mode-hook . (lambda () (treesit-parser-create 'dart))))

; (gpack lsp-dart
  ; :disable
  ; :hook (dart-mode-hook . (lambda () (require 'lsp-dart) (lsp-deferred))))

(provide 'spring-dart)
