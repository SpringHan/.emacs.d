;;;; This is the lsp-mode settings file

;;; Packages
(use-package lsp-bridge
  :init
  (git-download-ensure "lsp-bridge" "SpringHan/lsp-bridge" 5)
  (use-package cape)
  (use-package markdown-mode)

  :load-path "~/.emacs.d/third-party/lsp-bridge"
  :bind (("C-x C-l" . nil)
         ("C-x C-l g" . lsp-bridge-find-def-other-window)
         ("C-x C-l d" . lsp-bridge-popup-documentation)
         ("C-x C-l n" .   lsp-bridge-diagnostic-jump-next)
         ("C-x C-l p" .   lsp-bridge-diagnostic-jump-prev)
         ("C-x C-l j" . lsp-bridge-find-def)
         ("C-x C-l e" . lsp-bridge-toggle-sdcv-helper)
         ("C-x C-l l" .  lsp-bridge-diagnostic-list)
         ("C-x C-l a" . lsp-bridge-code-action)
         ("C-x C-l f" . lsp-bridge-code-format)
         ("C-x C-l r" . lsp-bridge-find-references)
         ("C-x C-l P" . lsp-bridge-peek)
         ("C-x C-l r" . lsp-bridge-rename)
         ("M-n" . lsp-bridge-popup-documentation-scroll-up)
         ("M-p" . lsp-bridge-popup-documentation-scroll-down)
         ("C-' R" . lsp-bridge-restart-process))
  :hook
  (after-init-hook . global-lsp-bridge-mode)
  (emacs-lisp-mode . (lambda () (lsp-bridge-mode -1)))

  :config
  (setq acm-enable-yas nil
        acm-enable-quick-access t
        lsp-bridge-enable-inlay-hint nil
        lsp-bridge-python-lsp-server 'pyright
        ;; (lsp-bridge-enable-diagnostics nil)
        lsp-bridge-diagnostic-fetch-idle 0.25)

  (define-key lsp-bridge-peek-keymap (kbd "M-l t") #'lsp-bridge-peek-through)

  (defun spring/kill-lsp-error ()
  "Insert a `.' sign to avoid unreal error caused by lsp-brideg."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (stringp (buffer-file-name (current-buffer)))
      (goto-char (line-end-position))
      (insert ".")
      (save-buffer)))))

(use-package citre
  :autoload citre-project-root
  :bind (("C-x c j" . citre-jump)
         ("C-x c J" . citre-jump-back)
         ("C-x c p" . citre-ace-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :config
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
   citre-prompt-language-for-ctags-command t))

;; (gpack flycheck
;;   :key (("C-x C-l p" . flycheck-previous-error)
;;         ("C-x C-l n" . flycheck-next-error)
;;         ("C-x C-l l" . flycheck-list-errors))
;;   :var (flycheck-auto-display-errors-after-checking . nil)
;;   :config (progn
;;             (add-to-list 'flycheck-checkers 'rustic-clippy)))
;; (gpack flycheck-rust
;;   :config (with-eval-after-load 'rust-mode
;;             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;; Dape
;; (use-package company)
(use-package dape)
;; (spring/extra-add-to-list "~/.emacs.d/third-party/dape/dape" t)
;; (spring/native-compile-or-load "~/.emacs.d/third-party/dape/dape" nil t)

(provide 'init-lsp)
