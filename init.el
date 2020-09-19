;;;;; SpringHan's Emacs Configuration
;;; Mirror Config
(require 'package)
(require 'cl-lib) ; Common Lisp
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; Variables
(defvar spring/time-block nil
	"If the the time-block changed, it is t.
Otherwise it's nil.")
(defvar spring/unwanted-buffer
	'("*dashboard*" "notes.org" "tasks.org" "user-init.el" "*Help*")
	"The buffers that I don't need.")

;;;; Other config files
;;; Error Capture
(load-file "~/.emacs.d/init-error-manager.el")
(require 'init-error-manager)
;;; Macros
(load-file "~/.emacs.d/macros.el")
;;; GitHub
(when (spring/error-check '("~/.emacs.d/init.el::17" "init.el") :file-exists "~/.emacs.d/token.el")
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
(setq default-tab-width 2) ; The tab width
(setq indent-tabs-mode t) ; Use tab indent
(setq css-indent-offset 2) ; Set css indent width
(setq display-time-24hr-format t) ; Display the time and date on modeline
(display-time-mode t) ; Display the time
(column-number-mode t) ; Show the column number in the modeline
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-init-file "~/.emacs.d/var/user-init.el")
(add-hook 'markdown-mode-hook #'markdown-table-keymap) ; Add the markdown table align keymap
(add-hook 'erc-mode-hook #'(lambda () (linum-mode -1)))

;;; Plugin requires
(require 'init-package)
(package-initialize)

;;; Enable Disbaled command
(require 'novice)
(load-file "~/.emacs.d/init-enable-disabled-commands.el")
(require 'init-enable-disabled-commands)
(enable-commands-init)

;;; Languages settings
(require 'spring-python)
