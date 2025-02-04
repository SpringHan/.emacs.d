;;; This is the file for rust.

(defvar rustic-lsp-setup-p nil)
(gpack rustic
  :hook (rustic-mode-hook . electric-indent-local-mode)
  :config
  (add-hook 'rustic-mode-hook (lambda () (treesit-parser-create 'rust))))

(gpack toml-mode)

(provide 'spring-rust)
