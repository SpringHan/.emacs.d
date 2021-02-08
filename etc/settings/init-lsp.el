;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
(gpack lsp-mode
  :hook ((c-mode-hook c++-mode-hook lisp-mode-hook) . lsp-deferred)
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

;;; lsp-ui
(gpack lsp-ui
  :hook (lsp-mode-hook . lsp-ui-mode)
  :var ((lsp-ui-sideline-enable . nil)
        (lsp-ui-peek-enable . t)
        (lsp-ui-doc-enable . t)
        (lsp-ui-doc-position . 'at-point))
  :config (progn
            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(gpack flycheck)

(defvar lsp-on-touch-time 0
  "The lsp-on-change's time.")

;;; Dap-mode
(gpack dap-mode
  :hook (lsp-mode-hook . dap-mode)
  :key ("C-' d" . dap-debug))

;;; Config
;; (eval-after-load 'lsp-mode
;;   '(progn
;;      (defvar lsp-on-touch-time 0)
;;      (defadvice lsp-on-change (around lsp-on-change-hack activate)
;;        ;; don't run `lsp-on-change' too frequently
;;        (when (> (- (float-time (current-time))
;;                    lsp-on-touch-time) 30)
;;          (setq lsp-on-touch-time (float-time (current-time)))
;;          ad-do-it))))

(provide 'init-lsp)
