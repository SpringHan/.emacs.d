;;;; Mirror Config
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;;;; Plugins
;;; Third-party
;; Emacs Application Framework
(add-to-list 'load-path "~/.emacs.d/emacs-application-framework")
(require 'eaf)
;; Atom-One
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'atom-one-dark t)
;;; In Mirror
;; Packages Installation
(defvar my/packages '(
			  ;; Org
			  org
			  ;; Terminal
			  vterm
			  ;; Nice Plugin
			  counsel
			  ivy
			  iedit
			  ;; mode-line
			  spaceline
			  ;; More
			  all-the-icons) "Default packages")
(setq package-selected-packages my/packages) ; Set the packages need to install
(require 'cl) ; The Lisp Extension
(defun my/packages-installed-p()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	(finally (return t)))) ; Define function to return whether package installed
(defun auto-download-plugins()
  (unless (my/packages-installed-p)
    (message "%s" "Refreshing package database")
    (package-refresh-contents)
    (dolist (pkg my/packages)
      (when (not (package-installed-p pkg))
	(package-install pkg))))) ; Install the packages haven't installed

;; Org
(require 'org)
(setq org-src-fontify-natively t) ; Org code highlight
;; Vterm
(require 'vterm)
;; Counsel
(require 'counsel)
;; Icons
(require 'all-the-icons)
;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
;; ivy
(require 'ivy)
(ivy-mode 1)


;;;; Other config files
;;; GitHub
(load-file "~/.emacs.d/token.el")
(require 'github-token)
;;; Other files
(add-to-list 'load-path "~/.emacs.d/etc/")
;; Org-mode
(require 'init-org)


;;;; Basic things
;;; UI
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode 1) ; Show the line number
(global-hl-line-mode 1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen 1) ; Close the start flash
(set-face-attribute 'default nil
		    :height 160
		    :family "Source Code Pro"
		    :weight 'normal
		    :width 'normal) ; Set the font size

;;; Functions
(delete-selection-mode 1) ; Delete the seleceted text
(setq make-backup-files nil) ; Don't let Emacs make up backup file
(setq auto-save-default nil) ; Don't auto save the file
(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
(setq default-tab-width 2) ; The tab width

;; Dired-mode
(require 'dired-x) ; Use dired-x to add the `C-x C-j` keymap
(put 'dired-find-alternate-file 'disabled nil) ; Don't let dired-mode create a new buffer for the dir
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; Abbrev-mode
(setq-default abbrev-mode t) ; Open abbrev-mode
(define-abbrev-table 'global-abbrev-table '(
					    ("MyName" "SpringHan")))

;; Open the configuration quickly (Function)
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun open-vterm()
  (interactive)
  (vterm)
  (linum-mode -1))


;; Keybindings
(define-prefix-command 'ctl-z-map)		 ; Create the C-z map
(global-set-key (kbd "C-z") 'ctl-z-map)		 ; Set the C-z
(global-set-key (kbd "C-z i") 'open-config-file) ; Open the init.el
(global-set-key (kbd "C-z p") 'package-list-packages) ; Open the package interface
(global-set-key (kbd "C-z d") 'auto-download-plugins) ; Auto download plugins
(global-set-key (kbd "C-z C-b") 'buffer-menu) ; Open the buffer menu
(global-set-key (kbd "C-z C-i") 'erc) ;Open the erc
(global-set-key (kbd "C-z C-w") 'eaf-open-browser) ; Open the eaf browser
(global-set-key (kbd "C-z C-m") 'eaf-open-bookmark) ; Open the eaf browser by bookmarks
(global-set-key (kbd "C-z C-t") 'open-vterm) ; Open vterm
(global-set-key (kbd "C-z C-p") 'previous-buffer) ; Goto previous buffer
(global-set-key (kbd "C-z C-n") 'next-buffer)	  ;Goto Next buffer
(global-set-key (kbd "C-z m") 'set-mark-command) ; The mark key map
(global-set-key (kbd "<f12>") 'tab-bar-mode) ; Open or close the tab-bar-mode


;; Plugin Setting
;;; Emacs Application Framework
(eaf-setq eaf-browser-remember-history "true")
;; (setq eaf-browser-default-search-engine 'Bing)
(eaf-setq eaf-browser-default-zoom "1.0")
(eaf-setq eaf-browse-blank-page-url "https://cn.bing.com/")
(eaf-setq eaf-browser-dark-mode "true")
;;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-z a") 'counsel-linux-app)
;;; Iedit
(global-set-key (kbd "C-z e") 'iedit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(spaceline all-the-icons counsel vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Check if the plugins have installed
(if (file-exists-p "~/.emacs.d/initialized")
    (message "The Plugins have already installed.")
  (auto-download-plugins)
  (make-empty-file "~/.emacs.d/initialized")
  (message "The Plugins have already installed."))
