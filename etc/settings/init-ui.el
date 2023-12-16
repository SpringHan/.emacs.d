;;;; This file is used for emacs UI
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(tab-bar-mode -1) ; Set tab bar not display
(blink-cursor-mode -1) ; Close cursor blink
(setq tab-bar-show nil) ; Always not display tab bar
;;; Set the line number as the relative style
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-hl-line-mode -1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(setq cursor-type 'box) ; Cursor Shape
(setq inhibit-splash-screen t) ; Close the start flash
(set-face-attribute
 'default nil
 :height 140
 :family "Monego"
 :weight 'normal
 :width 'normal) ; Set the font size
;; Set backgroup alpha
(unless (file-exists-p
         (expand-file-name (locate-user-emacs-file "not-alpha")))
  (set-frame-parameter nil 'alpha-background 75))

(setq-default truncate-lines t)

(provide 'init-ui)
