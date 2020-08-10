;;;; This file is used for packages configuration and more
;; Set the plugin keybinding
(define-prefix-command 'plugin-key)
(global-set-key (kbd "C-'") 'plugin-key)

;; Third-party
;; Emacs Application Framework
(add-to-list 'load-path "~/.emacs.d/third-party/emacs-application-framework")
(package-require
 'eaf
 '(("C-z C-w l" eaf-open-browser)
	 ("C-z C-w h" eaf-open-browser-with-history)
	 ("C-z C-m b" eaf-open-bookmark))
 :hooks
 :config
 :outside)
(progn
	(eaf-setq eaf-browser-remember-history "true")
	(eaf-setq eaf-browser-default-zoom "1.0")
	(eaf-setq eaf-browser-dark-mode "true"))

;; Org
(package-require
 'org
 '(
   ("C-z g" org-agenda)
   ("C-z C-c" org-capture))
 :hooks
 (progn
   (setq org-src-fontify-natively t)
   (require 'init-org)
   (package-require
    'org-bullets
    :keymaps
    '(org-mode-hook org-bullets-mode)
    (setq org-bullets-bullet-list '("" "☯" "" "" )))))

;; Vterm
(package-require
 'vterm
 '(("C-' C-t" open-vterm)))

;; Counsel
(package-require
 'counsel
 '(
   ("M-x" counsel-M-x)
   ("C-x C-f" counsel-find-file)
   ("C-z a" counsel-linux-app)))

;; Icons
(package-require
 'all-the-icons
 '(("C-' C-i" all-the-icons-insert)))

;; Spaceline
;; (package-require
;;  'spaceline-config
;;  :keymaps
;;  :hooks
;;  (spaceline-spacemacs-theme))

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
(package-require
 'ace-window
 '(("C-' C-c" ace-window)))

;; Calendar-China
(package-require 'cal-china-x)

;; Dascboard
(package-require
 'dashboard
 :keymaps
 :hooks
 (progn
	 (dashboard-setup-startup-hook)
	 (setq dashboard-banner-logo-title "Live in Emacs!")
	 (setq dashboard-startup-banner 'logo)
	 (setq dashboard-center-content t
				 dashboard-set-heading-icons t
				 dashboard-set-navigator t)))

;; Iedit
(package-require
 'iedit
 '(("C-' C-e" iedit-mode)))

;; hungry-delete
(package-require
 'hungry-delete
 '(("C-' C-h" hungry-delete-mode)) 
 '((emacs-lisp-mode-hook lisp-mode-hook) hungry-delete-mode))

;; js2-mode
(package-require
 'js2-mode
 :keymaps
 '(js-mode js2-mode))

;; Web-mode
(package-require
 'web-mode
 :keymaps
 :hooks
 (progn
   (setq auto-mode-alist
	 (append
	  '(("\\.html\\'" . web-mode))
	  auto-mode-alist))
     (setq-default web-mode-markup-indent-offset 2 ; Indent of HTML
									 web-mode-css-indent-offset 2
									 web-mode-code-indent-offset 2) ; Indent of JavaScript in HTML
     ))


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
   (package-require
		'company-c-headers
		:keymaps
		:hooks
		(add-to-list 'company-backends 'company-c-headers))))

;; Lsp-mode
(package-require
 'lsp-mode
 :keymaps
 '((c-mode-hook python-mode c++-mode-hook lisp-mode-hook js-mode-hook web-mode-hook emacs-lisp-mode-hook) lsp-mode))

;; ccls (For lsp-mode)
(package-require 'ccls)

;; emmet-mode
(package-require
 'emmet-mode
 :keymaps
 '(web-mode-hook emmet-mode)
 (progn
	 (setq emmet-self-closing-tag-style " /")))

;; Snippet
(package-require
 'yasnippet
 '(("C-' i" yas-insert-snippet)
	 ("C-' C-y" yas-expand-from-trigger-key))
 '(after-init-hook yas-global-mode)
 (progn
	 (package-require
		'yasnippet-snippets)
	 (setq yas-snippet-dirs '(
														"~/.emacs.d/snippets"
														"~/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets"))))

;;FlyMake
(package-require
 'flymake
 '(("C-' C-f" flymake-mode)))

;; rainbow-delimiters
(package-require
 'rainbow-delimiters
 :keymaps
 '((lisp-mode-hook emacs-lisp-mode-hook org-mode-hook) rainbow-delimiters-mode))

;; indent guide
(package-require
 'indent-guide
 :keymaps
 '(after-init-hook indent-guide-global-mode))

;; doom-modeline
(package-require
 'doom-modeline
 :keymaps
 '(after-init-hook doom-modeline-mode)
 (setq-default doom-modeline-height 18))

;; magit
(package-require
 'magit
 '(("C-' m" magit-status)))

;; Window Resize
(package-require
 'windresize
 '(("C-' C-r" windresize)
	 ("C-' SPC" windresize-exit)))

;; multiple cursor
(package-require
 'multiple-cursors
 '(("C-M-l" mc/edit-lines)
	 ("C->" mc/mark-next-like-this)
	 ("C-<" mc/mark-previous-like-this)
	 ("M-m" newline)))

;; youdao translate
(package-require
 'youdao-dictionary
 '(("C-' t" youdao-dictionary-search-at-point)))

;; Treemacs : File explore
(package-require
 'treemacs
 '(("C-' e" treemacs)))

(provide 'init-package)
