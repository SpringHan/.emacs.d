;;;; This file is used for python configuration

;;; Indent
(setq python-indent-offset 2)

;;; Completing
(package-require
 'lsp-pyright
 :hook '(python-mode-hook . (lambda () (require 'lsp-pyright) (lsp)))
 :delay-eval '(when (executable-find "python3")
								(setq lsp-pyright-python-executable-cmd "python3")))

(provide 'spring-python)
