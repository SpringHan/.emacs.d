;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
(gpack nox
  :repo "manateelazycat/nox"
  :var (nox-optimization-p . nil)
  :key (("C-' D" . nox-show-doc)))

(gpack flycheck)

(gpack lsp-mode
  :hook (web-mode-hook . (lambda ()
                           (lsp-deferred)
                           (setq-local company-backends
                                       (append '(company-capf) company-backends))))
  :key ("C-' F" . lsp-format-buffer)
  :var ((lsp-idle-delay . 0.2)
        (lsp-enable-indentation . t)
        (lsp-keep-workspace-alive . nil)
        (lsp-auto-guess-root . nil)
        (lsp-file-watch-threshold . 1000)
        (lsp-eldoc-hook . nil)
        (lsp-log-io . nil)
        (lsp-enable-folding . nil)
        (lsp-enable-snippet . t)
        (lsp-prefer-flymake . :none)
        (lsp-completion-provider . :capf)))

;;; Dap-mode
(gpack dap-mode
  :key ("C-' d" . dap-debug))

(provide 'init-lsp)
