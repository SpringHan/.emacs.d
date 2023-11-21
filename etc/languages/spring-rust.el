;;; This is the file for rust.

(defvar rustic-lsp-setup-p nil)
(gpack rustic
  :hook (rustic-mode-hook . electric-indent-local-mode))

(provide 'spring-rust)
