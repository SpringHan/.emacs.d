;;;;; SpringHan's Emacs Configuration

;;;; Mirror Config
(require 'package)
(require 'cl) ; The Lisp Extension
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;;; Third-party
;; Atom-One
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'atom-one-dark t)


;;;; Other config files
;; Macros
(load-file "~/.emacs.d/macros.el")
;;; GitHub
(load-file "~/.emacs.d/token.el")
(require 'github-token)
;;; Other files
(add-to-list 'load-path "~/.emacs.d/etc/")
;; UI
(require 'init-ui)
;; The functions
(require 'init-functions)
;; Keymaps
(require 'init-keymaps)
;; Other mode settings
(require 'init-modes)
;; Packages
(require 'init-require-package)


;;;; Basic things
;;; Functions
(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
(delete-selection-mode t) ; Delete the seleceted text
(show-paren-mode t) ; Highlight the ()
(electric-pair-mode t) ; Auto complete the ()
(setq cursor-type 'box) ; Set the cursor as a box
(setq make-backup-files nil ; Don't let Emacs make up backup file
      create-lockfiles nil ;Don't make lockfile
      auto-save-default nil ; Don't auto save the file
      )
(setq-default tab-width 2) ; The tab width
(setq-default indent-tabs-mode t) ; Use tab indent
(setq-default css-indent-offset 2) ; Set css indent width
(display-time-mode t) ; Display the time
(setq display-time-24hr-format t) ; Display the time and date on modeline
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-emacs-directory "~/.emacs.d/var")
(setq user-init-file "~/.emacs.d/var/user-init.el")

;;;; Plugin requires
(require 'init-package)
