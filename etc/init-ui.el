;;;; This file is used for emacs UI
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode 1) ; Show the line number
(global-hl-line-mode -1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen 1) ; Close the start flash
(set-face-attribute 'default nil
		    :height 160
		    :family "Source Code Pro"
		    :weight 'normal
		    :width 'normal) ; Set the font size

(provide 'init-ui)
