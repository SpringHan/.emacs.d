;;;; This file is used for packages configuration and more
;; Org
(package-require
 'org
 '(
   ("C-z g" org-agenda)
   ("C-z C-c c" org-capture))
 :hooks
 (progn
   (setq org-src-fontify-natively t)
   (require 'init-org)))

;; Vterm
(package-require
 'vterm
 '(("C-z C-t" open-vterm)))

;; Counsel
(package-require
 'counsel
 '(
   ("M-x" counsel-M-x)
   ("C-x C-f" counsel-find-file)
   ("C-z a" counsel-linux-app)))

;; Icons
(package-require 'all-the-icons)

;; Spaceline
(package-require
 'spaceline-config
 :keymaps
 :hooks
 (spaceline-spacemacs-theme))

;; ivy
(package-require
 'ivy
 :keymaps
 '(after-init-hook ivy-mode))

;; Bongo
(package-require 'bongo)

;; Which Key
(package-require
 'which-key
 :keymaps
 '(after-init-hook which-key-mode))

;; ace window
(package-require 'ace-window)

;; Calendar-China
(package-require 'cal-china-x)

;; Dascboard
(package-require 'dashboard)

;; Iedit
(package-require
 'iedit
 '(("C-z e" iedit-mode)))

;; hungry-delete
(package-require
 'hungry-delete
 '(("C-z h" hungry-delete-mode)))

;; js2-mode
(package-require
 'js2-mode
 :keymaps
 '(js-mode js2-mode))

;; Web-mode
(package-require
 'web-mode
 :keymaps
 '(web-mode web-mode-indent-setup)
 (progn
   (setq auto-mode-alist
	 (append
	  '(("\\.html\\'" . web-mode))
	  auto-mode-alist))
   (defun web-mode-indent-setup()
     (setq web-mode-markup-indent-offset 2
	   web-mode-css-indent-offset 2
	   web-mode-code-indent-offset 2) ; Indent of JavaScript in HTML
     )))


;;; Auto Completion
;; Company (Complete Anything)
(package-require
 'company
 :keymaps
 '(after-init-hook global-company-mode)
 (progn
   (setq company-idle-delay 0)
   (with-eval-after-load
       'company
     (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "M-n") nil)
     (define-key company-active-map (kbd "C-n") #'company-select-next)
     (define-key company-active-map (kbd "C-p") #'company-select-previous))
   (package-require 'company-lsp)
   (package-require 'company-c-headers)
   (push 'company-lsp company-backends)))

;; Lsp-mode
(package-require
 'lsp-mode
 :keymaps
 '((c-mode-hook python-mode c++-mode-hook lisp-mode-hook js-mode-hook web-mode-hook emacs-lisp-mode-hook) lsp-mode))

;; ccls (For lsp-mode)
(package-require 'ccls)


;; Auto-yasnippet
(package-require
 'auto-yasnippet
 '(("M-n" aya-create)
   ("M-p" aya-expand)))

;;FlyMake
(package-require
 'flymake
 '(("C-z f" flymake-mode)))

;; rainbow-delimiters
(package-require
 'rainbow-delimiters
 :keymaps
 '((lisp-mode-hook emacs-lisp-mode-hook) rainbow-delimiters-mode))

(provide 'init-package)
