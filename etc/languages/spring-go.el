;;;; The go setup for my emacs.

(gpack go-mode
  :hook (go-mode-hook . (lambda () (setq-local indent-tabs-mode nil)))
  :config
  (sniem-object-catch-mode-defalist go-mode
    ("`" . "`")))

(provide 'spring-go)
