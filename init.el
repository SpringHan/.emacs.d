;;;; Plugins
;;; Third-party
;; Emacs Application Framework
(add-to-list 'load-path "~/.emacs.d/emacs-application-framework")
(require 'eaf)
;; Vterm
(add-to-list 'load-path "~/.emacs.d/emacs-libvterm")
(require 'vterm)
;; Atom-One
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'atom-one-dark t)
;;; Locale
(require 'org)
(setq org-src-fontify-natively t) ; Org code highlight

;;; Basic things
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode 1) ; Show the line number
(delete-selection-mode 1) ; Delete the seleceted text
(global-hl-line-mode 1) ; Highlight the current line
(setq make-backup-file nil) ; Don't let Emacs make up backup file
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen 1) ; Close the start flash
;;(setq fonts '("Source Code Pro"))
(set-face-attribute 'default nil :height 160) ; Set the font size
;; (set-default-font "Source Code Pro-24") ; Set font
;; Open the configuration quickly (Function)
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f1>") 'open-config-file) ; The keybinding of the function


;; Keybindings
(define-prefix-command 'ctl-z-map) ; Create the C-z map
(global-set-key (kbd "C-z") 'ctl-z-map) ; Set the C-z
(global-set-key (kbd "C-z C-b") 'buffer-menu) ; Open the buffer menu
(global-set-key (kbd "C-z C-i") 'erc) ;Open the erc
(global-set-key (kbd "C-z C-w") 'eaf-open-browser) ; Open the eaf browser
(global-set-key (kbd "C-z C-m") 'eaf-open-bookmark) ; Open the eaf browser by bookmarks
(global-set-key (kbd "C-z C-t") 'vterm) ; Open vterm
(define-key global-map [C-return] 'set-mark-command) ; The mark key map
