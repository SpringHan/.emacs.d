;;; This file is used for python configuration

;;; Indent
(setq-default python-indent-offset 2)

;;; Completing
(gpack lsp-pyright
  :hook (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config (when (executable-find "python3")
            (setq lsp-pyright-python-executable-cmd "python3")))

(gpack lsp-python-ms
  :disable
  :config (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode-hook . (lambda ()
                              (require 'lsp-python-ms)
                              (lsp))))

(gpack live-py-mode)

(gpack elpy
  :disable
  :config (with-eval-after-load 'python
            (unless (or (not buffer-file-name)
                        (string= (file-name-extension buffer-file-name) "org"))
              (elpy-enable))))

(provide 'spring-python)
