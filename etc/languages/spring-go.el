;;;; The go setup for my emacs.

(use-package go-mode
  :hook (go-mode . (lambda () (setq-local indent-tabs-mode nil)
                     (electric-indent-local-mode t)))
  :config
  (with-eval-after-load 'sniem
    (sniem-object-catch-mode-defalist go-mode
      ("`" . "`"))))

(provide 'spring-go)
