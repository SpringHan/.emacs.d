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
;; Org
(require 'org)
(setq org-src-fontify-natively t) ; Org code highlight
;; Vterm
(require 'vterm)
;; Counsel
(require 'counsel)
;; Icons
(require 'all-the-icons)

;;; Basic things
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode 1) ; Show the line number
(delete-selection-mode 1) ; Delete the seleceted text
(global-hl-line-mode 1) ; Highlight the current line
(setq initial-frame-alist '((fullscreen . maximized))) ; Initial fullscreen
(setq make-backup-file nil) ; Don't let Emacs make up backup file
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen 1) ; Close the start flash
;;(setq fonts '("Source Code Pro"))
(set-face-attribute 'default nil
		    :height 160
		    :family "Source Code Pro"
		    :weight 'normal
		    :width 'normal) ; Set the font size
;; (set-default-font "Source Code Pro-24") ; Set font
;; Open the configuration quickly (Function)
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun open-vterm()
  (interactive)
  (vterm)
  (hl-line-mode -1)
  (linum-mode -1))


;; Keybindings
(define-prefix-command 'ctl-z-map) ; Create the C-z map
(global-set-key (kbd "C-z") 'ctl-z-map) ; Set the C-z
(global-set-key (kbd "C-z i") 'open-config-file) ; Open the init.el
(global-set-key (kbd "C-z p") 'package-list-packages) ; Open the package interface
(global-set-key (kbd "C-z C-b") 'buffer-menu) ; Open the buffer menu
(global-set-key (kbd "C-z C-i") 'erc) ;Open the erc
(global-set-key (kbd "C-z C-w") 'eaf-open-browser) ; Open the eaf browser
(global-set-key (kbd "C-z C-m") 'eaf-open-bookmark) ; Open the eaf browser by bookmarks
(global-set-key (kbd "C-z C-t") 'open-vterm) ; Open vterm
(define-key global-map [C-return] 'set-mark-command) ; The mark key map


;; Plugin Setting
;;; Emacs Application Framework
(eaf-setq eaf-browser-remember-history "true")
(setq eaf-browser-default-search-engine 'Bing)
(eaf-setq eaf-browse-blank-page-url "https://cn.bing.com/")
;;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(all-the-icons counsel vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
