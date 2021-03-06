;;; This file is used for python configuration

;;; Indent
(setq-default python-indent-offset 2)

;;; Completing
(gpack lsp-pyright
  :hook (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config (when (executable-find "python3")
            (setq lsp-pyright-python-executable-cmd "python3")))

(gpack live-py-mode)

(provide 'spring-python)
