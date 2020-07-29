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
;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
;; ivy
(require 'ivy)
(ivy-mode 1)
;; GitHub
(load "~/.emacs.d/token.el")
(require 'github-token)

;;; Basic things
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode 1) ; Show the line number
(delete-selection-mode 1) ; Delete the seleceted text
(global-hl-line-mode 1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(setq make-backup-files nil) ; Don't let Emacs make up backup file
(setq auto-save-default nil) ; Don't auto save the file
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen 1) ; Close the start flash
(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
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
;; (setq eaf-browser-default-search-engine 'Bing)
(eaf-setq eaf-browser-default-zoom "1.0")
(eaf-setq eaf-browse-blank-page-url "https://cn.bing.com/")
(eaf-setq eaf-browser-dark-mode "true")
;;; Counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-z a") 'counsel-linux-app)

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
