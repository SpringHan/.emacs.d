;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
(gpack nox
  :repo "manateelazycat/nox"
  :var (nox-optimization-p . nil))

(gpack flycheck)

;;; Dap-mode
(gpack dap-mode
  :key ("C-' d" . dap-debug))

(provide 'init-lsp)
