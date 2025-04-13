;;; Haskell

(use-package haskell-mode
  :config
  (with-eval-after-load 'sniem
    (sniem-object-catch-mode-defalist haskell-mode
      ("`" . "`"))))

(provide 'spring-haskell)
