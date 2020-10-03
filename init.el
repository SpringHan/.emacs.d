;;;;; SpringHan's Emacs Configuration
;;; Mirror Config
(require 'package)
(require 'cl-lib) ; Common Lisp
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; GC
(setq gc-cons-threshold (* 50 1024 1024))

;;; Variables
(defvar spring/time-block nil
	"If the the time-block changed, it is t.
Otherwise it's nil.")
(defvar spring/unwanted-buffer
	'("*dashboard*" "notes.org" "tasks.org" "user-init.el" "*Help*" "*Backtrace*")
	"The buffers that I don't need.")

;;;; Other config files
(add-to-list 'load-path "~/.emacs.d/etc/tools")
;;; Error Capture
(require 'init-error-manager)
;;; GitHub
(when (spring/error-check '("~/.emacs.d/init.el::21" "init.el") :file-exists "~/.emacs.d/token.el")
	(load-file "~/.emacs.d/token.el")
	(require 'github-token))
;;; The cache directory
(setq user-emacs-directory "~/.emacs.d/var")
;;; Other files
(add-to-list 'load-path "~/.emacs.d/etc/settings")
(add-to-list 'load-path "~/.emacs.d/etc/languages")
;;; UI
(require 'init-ui)
;;; Packages
(require 'init-require-package)
;;; The functions
(require 'init-functions)
;;; Keymaps
(require 'init-keymaps)
;;; Other mode settings
(require 'init-modes)


;;; Basic things
;;; Functions
(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
(delete-selection-mode t) ; Delete the seleceted text
(show-paren-mode t) ; Highlight the ()
(electric-pair-mode t) ; Auto complete the ()
(setq electric-pair-pairs
			'((?\" . ?\")
				(?\( . ?\))
				(?\< . ?\>)
				(?\{ . ?\}))) ; Set the electric-pair-mode's pair keywords
(setq make-backup-files nil ; Don't let Emacs make up backup file
      create-lockfiles nil ;Don't make lockfile
      auto-save-default nil ; Don't auto save the file
      )
(setq initial-scratch-message
			";; Spring Emacs
;; This is the scratch buffer for you.

")
(setq-default tab-width 2) ; The tab width
(setq-default indent-tabs-mode t) ; Use tab indent
(setq-default css-indent-offset 2) ; Set css indent width
(setq display-time-24hr-format t) ; Display the time and date on modeline
(display-time-mode t) ; Display the time
(column-number-mode t) ; Show the column number in the modeline
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-init-file "~/.emacs.d/var/user-init.el")
(add-hook 'markdown-mode-hook #'(lambda ()
																	(define-key markdown-mode-map (kbd "C-c C-c TAB") 'markdown-table-align))) ; Add the markdown table align keymap
(add-hook 'erc-mode-hook #'(lambda () (linum-mode -1)))	; Close the line number in the erc mode
(add-hook 'magit-mode-hook #'(lambda () (define-key magit-mode-map "q" 'spring/kill-magit))) ; Define the function to kill the magit buffers
(add-hook 'shell-mode-hook #'(lambda () (company-mode -1) (define-key shell-mode-map (kbd "C-c l") 'spring/shell-clear)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (outline-minor-mode t)))
(add-hook 'web-mode-hook #'(lambda () (outline-minor-mode t)))


;;; Plugin requires
(require 'init-package)
(package-initialize)

;;; Tools
(require 'task-reminder)
(require 'init-macros)
(require 'init-enable-disabled-commands)
(enable-commands-init)


;;; Languages settings
(require 'spring-python)
(require 'spring-c)
