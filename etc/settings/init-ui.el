;;;; This file is used for emacs UI
(tab-bar-mode -1)      ; Set tab bar not display
(blink-cursor-mode -1) ; Close cursor blink
(pixel-scroll-mode)
(setq tab-bar-show nil) ; Always not display tab bar
(setq inhibit-splash-screen t) ; Close the start flash

;;; Line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-hl-line-mode -1) ; Highlight the current line

;;; Cursor shape
(setq cursor-type 'box)

;;; Font
(set-face-attribute
 'default nil
 :height 120
 :family "Monego"
 :weight 'normal
 :width 'normal) ; Set the font size

;; Set backgroup tranparency
(unless (file-exists-p
         (expand-file-name (locate-user-emacs-file "not-alpha")))
  (set-frame-parameter nil 'alpha-background 75))

(provide 'init-ui)
