;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
;; (gpack nox
;;   :repo "manateelazycat/nox"
;;   :var (nox-optimization-p . nil)
;;   :key (("C-' D" . nox-show-doc))
;;   :config)

;; (gpack flycheck)

(gpack citre
  :key (("C-x c j" . citre-jump)
        ("C-x c J" . citre-jump-back)
        ("C-x c p" . citre-ace-peek)
        ("C-x c u" . citre-update-this-tags-file))
  :config
  (progn
    (require 'citre-config)
    (setq
     ;; Set these if readtags/ctags is not in your path.
     citre-readtags-program "/usr/bin/readtags"
     citre-ctags-program "/usr/bin/ctags"
     ;; Set this if you use project management plugin like projectile.  It's
     ;; used for things like displaying paths relatively, see its docstring.
     citre-project-root-function #'projectile-project-root
     ;; Set this if you want to always use one location to create a tags file.
     citre-default-create-tags-file-location 'project-cache
     ;; See the "Create tags file" section above to know these options
     citre-use-project-root-when-creating-tags t
     citre-prompt-language-for-ctags-command t)
    (add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                        (setq-local company-backends
                                                    (append '(company-elisp) company-backends))))))

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
