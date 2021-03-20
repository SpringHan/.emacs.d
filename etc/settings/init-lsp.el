;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
(gpack nox
  :repo "manateelazycat/nox"
  :var (nox-optimization-p . nil)
  :key (("C-' D" . nox-show-doc)))

(gpack flycheck)

;;; Dap-mode
(gpack dap-mode
  :key ("C-' d" . dap-debug))

(provide 'init-lsp)
