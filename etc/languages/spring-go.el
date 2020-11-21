;;;; The go setup for my emacs.

(gpack go-mode
  :hook (go-mode-hook . lsp))

(provide 'spring-go)
