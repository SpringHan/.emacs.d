;;; This is the file for rust.

(use-package rustic
  :init (defvar rustic-lsp-setup-p nil)
  :hook (rustic-mode . electric-indent-local-mode)
  :config
  (use-package toml-mode)
  (add-hook 'rustic-mode-hook (lambda () (treesit-parser-create 'rust))))

(provide 'spring-rust)
