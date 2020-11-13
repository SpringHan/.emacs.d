;;;; The go setup for my emacs.

(package-require go-mode
	:hook (go-mode-hook . lsp))

(provide 'spring-go)
