;;;; This file is used for emacs UI
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(global-linum-mode t) ; Show the line number
(global-hl-line-mode -1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(set-frame-parameter nil 'alpha '(90 . 100))
(setq cursor-type 'bar) ; Cursor Shape
(setq inhibit-splash-screen t) ; Close the start flash
(set-face-attribute
 'default nil
 :height 160
 :family "Source Code Pro"
 :weight 'normal
 :width 'normal) ; Set the font size

(provide 'init-ui)
