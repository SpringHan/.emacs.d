;;;; This is the lsp-mode settings file

;; ;;; Packages

(gpack cape)
(gpack markdown-mode)
(gpack lsp-bridge
  :repo "manateelazycat/lsp-bridge"
  :key (("C-x C-l" . nil)
        ("C-x C-l g" . lsp-bridge-find-def-other-window)
        ("C-x C-l d" . lsp-bridge-popup-documentation)
        ("C-x C-l n" .   lsp-bridge-diagnostic-jump-next)
        ("C-x C-l p" .   lsp-bridge-diagnostic-jump-prev)
        ("C-x C-l j" . lsp-bridge-find-def)
        ("C-x C-l e" . lsp-bridge-toggle-sdcv-helper)
        ("C-x C-l l" .  lsp-bridge-diagnostic-list)
        ("M-n" . lsp-bridge-popup-documentation-scroll-up)
        ("M-p" . lsp-bridge-popup-documentation-scroll-down))
  :hook (after-init-hook . global-lsp-bridge-mode)
  :var (acm-enable-yas . nil))

(spring/extra-add-to-list "~/.emacs.d/third-party/lsp-bridge/lsp-bridge" t)

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

;;; Voyager
;; (gpack voyager
;;   :repo "manateelazycat/voyager")

;;; Dape
(gpack company)
(gpack dape)
;; (spring/extra-add-to-list "~/.emacs.d/third-party/dape/dape" t)
;; (spring/native-compile-or-load "~/.emacs.d/third-party/dape/dape" nil t)

(provide 'init-lsp)
