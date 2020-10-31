;;;; This file is used for python configuration

;;; Indent
(setq-default python-indent-offset 2)

;;; Completing
(package-require 'lsp-pyright
	:hook '(python-mode-hook . (lambda () (require 'lsp-pyright) (lsp))))

;;; Folding
(add-hook 'python-mode-hook #'(lambda () (outline-minor-mode t)))

(provide 'spring-python)
