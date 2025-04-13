;;; This file is used to config the dart.

;;; Highlighting
(use-package dart-mode
  :hook (dart-mode . (lambda () (treesit-parser-create 'dart))))

(provide 'spring-dart)
