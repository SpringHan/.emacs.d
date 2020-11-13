;;;; This file is used for other modes

;;; Packages
;;; Dired-mode
(require 'dired-x) ; Use dired-x to add the `C-x C-j` keymap
(put 'dired-find-alternate-file 'disabled nil) ; Don't let dired-mode create a new buffer for the dir
(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;;; js2-mode
(package-require js2-mode
	:hook (js-mode . js2-mode))

;;; Web-mode
(package-require web-mode
	:config (progn
						(setq auto-mode-alist
									(append '(("\\.html\\'" . web-mode)) auto-mode-alist))
						(setq-default web-mode-markup-indent-offset 2 ; Indent of HTML
													web-mode-css-indent-offset 2
													web-mode-code-indent-offset 2) ; Indent of JavaScript in HTML
						))

;;; emmet-mode
(package-require emmet-mode
	:hook (web-mode-hook . emmet-mode)
	:var (emmet-self-closing-tag-style " /"))

;; Abbrev-mode
(setq-default abbrev-mode t) ; Open abbrev-mode
(define-abbrev-table
	'global-abbrev-table
	'(("MyName" "SpringHan")
		("MyEmail" "<springchohaku@qq.com>")
		("Copyright" "Copyright (C) <+++> SpringHan")))

;; Eshell-mode
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))

(provide 'init-modes)
