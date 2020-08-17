;;;;; SpringHan's Emacs Configuration
;;;; Mirror Config
(require 'package)
(require 'cl) ; Common Lisp
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)


;;;; Other config files
;; Error Capture
(load-file "~/.emacs.d/init-error-manager.el")
(require 'init-error-manager)
;; Macros
(load-file "~/.emacs.d/macros.el")
;;; GitHub
(when (spring/error-check '("~/.emacs.d/init.el::17" "init.el") :file-exists "~/.emacs.d/token.el")
	(load-file "~/.emacs.d/token.el")
	(require 'github-token))
;;; Other files
(add-to-list 'load-path "~/.emacs.d/etc/")
;; UI
(require 'init-ui)
;; Packages
(require 'init-require-package)
;; The functions
(require 'init-functions)
;; Keymaps
(require 'init-keymaps)
;; Other mode settings
(require 'init-modes)


;;;; Basic things
;;; Functions
(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
(delete-selection-mode t) ; Delete the seleceted text
(show-paren-mode t) ; Highlight the ()
(electric-pair-mode t) ; Auto complete the ()
(save-place-mode t) ; Save the point place
(setq electric-pair-pairs
			'((?\" . ?\")
				(?\( . ?\))
				(?\< . ?\>)
				(?\{ . ?\}))) ; Set the electric-pair-mode's pair keywords
(setq cursor-type 'box) ; Set the cursor as a box
(setq make-backup-files nil ; Don't let Emacs make up backup file
      create-lockfiles nil ;Don't make lockfile
      auto-save-default nil ; Don't auto save the file
      )
(setq-default tab-width 2) ; The tab width
(setq-default indent-tabs-mode t) ; Use tab indent
(setq-default css-indent-offset 2) ; Set css indent width
(setq display-time-24hr-format t) ; Display the time and date on modeline
(display-time-mode t) ; Display the time
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-emacs-directory "~/.emacs.d/var")
(setq user-init-file "~/.emacs.d/var/user-init.el")
(add-hook 'markdown-mode-hook #'markdown-table-keymap) ; Add the markdown table align keymap

;;;; Plugin requires
(require 'init-package)
