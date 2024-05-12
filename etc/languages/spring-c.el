;;;; This file is the c configuration.

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-basic-offset 2)

(add-hook 'c-mode-hook #'(lambda () (outline-minor-mode t)
                           (electric-indent-local-mode t)))
(add-hook 'c-mode-hook (lambda () (treesit-parser-create 'c)))
(add-hook 'c++-mode-hook (lambda () (treesit-parser-create 'cpp)))

(provide 'spring-c)
