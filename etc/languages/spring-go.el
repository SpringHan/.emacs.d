;;;; The go setup for my emacs.

(gpack go-mode
  :hook (go-mode-hook . nox-ensure))

(provide 'spring-go)
