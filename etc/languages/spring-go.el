;;;; The go setup for my emacs.

(gpack go-mode
  :hook ((go-mode-hook . nox-ensure)
         (go-mode-hook . (lambda () (setq-local indent-tabs-mode nil)))))

(provide 'spring-go)
