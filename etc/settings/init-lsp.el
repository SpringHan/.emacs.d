;;;; This is the lsp-mode settings file

;; ;;; Packages
;; ;;; Lsp-mode
;; (gpack nox
;;   :repo "manateelazycat/nox"
;;   :var (nox-optimization-p . nil)
;;   :key (("C-' D" . nox-show-doc))
;;   :config (setf (car nox-server-programs) '(rust-mode nox-rls "rust-analyzer")))

;; (spring/extra-add-to-list "~/.emacs.d/third-party/nox/nox" t)

(gpack lsp-bridge
  :repo "manateelazycat/lsp-bridge"
  :config
  (progn
    (require 'lsp-bridge-orderless)
    (require 'lsp-bridge-icon)
    (defun spring/lsp-bridge ()
      "Start lsp-bridge."
      (interactive)
      (with-current-buffer (current-buffer)
        (setq-local corfu-auto nil)
        (lsp-bridge-mode 1)))
    (global-set-key (kbd "<f3>") #'spring/lsp-bridge)))

;; (gpack flycheck)

(gpack citre
  :key (("C-x c j" . citre-jump)
        ("C-x c J" . citre-jump-back)
        ("C-x c p" . citre-ace-peek)
        ("C-x c u" . citre-update-this-tags-file))
  :config
  (progn
    (citre-auto-enable-citre-mode)
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
     citre-prompt-language-for-ctags-command t)))

;; (gpack lsp-mode
;;   :key (("C-' F" . lsp-format-buffer)
;;         ("C-' i" . lsp-treemacs-errors-list))
;;   :var ((lsp-idle-delay . 0.2)
;;         (lsp-enable-indentation . t)
;;         (lsp-keep-workspace-alive . nil)
;;         (lsp-auto-guess-root . nil)
;;         (lsp-file-watch-threshold . 1000)
;;         (lsp-eldoc-hook . nil)
;;         (lsp-log-io . nil)
;;         (lsp-enable-folding . nil)
;;         (lsp-enable-snippet . t)
;;         (lsp-prefer-flymake . :none)
;;         (lsp-completion-provider . :capf)))

;; (gpack lsp-ui
;;   :var (lsp-ui-doc-show-with-mouse . nil))

;;; Dap-mode
(gpack dap-mode
  :key ("C-' d" . dap-debug))

(provide 'init-lsp)
