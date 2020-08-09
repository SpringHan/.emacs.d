;;;;; SpringHan's Emacs Configuration

;;;; Mirror Config
(require 'package)
(require 'cl) ; The Lisp Extension
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;;;; Plugins Statement
;;; Third-party
;; Emacs Application Framework
(add-to-list 'load-path "~/.emacs.d/third-party/emacs-application-framework")
(require 'eaf)
;; (package-require
;; 'eaf
;; :keymaps
;; :hooks
;; :config
;; :path "~/.emacs.d/third-party/emacs-application-framework")
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
(delete-selection-mode 1) ; Delete the seleceted text
(show-paren-mode 1) ; Highlight the ()
(electric-pair-mode 1) ; Auto complete the ()
(setq cursor-type 'box) ; Set the cursor as a box
(setq make-backup-files nil ; Don't let Emacs make up backup file
      create-lockfiles nil ;Don't make lockfile
      auto-save-default nil ; Don't auto save the file
      )
(setq-default tab-width 2) ; The tab width
(setq-default indent-tabs-mode t) ; Use tab indent
(setq-default css-indent-offset 2) ; Set css indent width
(setq-default display-time-24hr-format t
							display-time-day-and-date t) ; Display the time and date on modeline
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-emacs-directory "~/.emacs.d/var")
(setq user-init-file "~/.emacs.d/var/user-init.el")


;;; Third Party's config
;;; Emacs Application Framework
(eaf-setq eaf-browser-remember-history "true")
;; (setq eaf-browser-default-search-engine 'Bing)
(eaf-setq eaf-browser-default-zoom "1.0")
(eaf-setq eaf-browse-blank-page-url "https://cn.bing.com/")
(eaf-setq eaf-browser-dark-mode "true")

;;;; Plugin requires
(require 'init-package)
