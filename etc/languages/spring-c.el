;;;; This file is the c configuration.

(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-basic-offset 2)

(add-hook 'c-mode-hook #'(lambda () (outline-minor-mode t)))
(add-hook 'c-mode-hook #'nox-ensure)
(add-hook 'c++-mode-hook #'nox-ensure)

(provide 'spring-c)
