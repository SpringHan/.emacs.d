;;;; This file is used for other modes

;;; Packages
;;; Dired-mode
(require 'dired-x) ; Use dired-x to add the `C-x C-j` keymap
(define-key dired-mode-map "q" 'kill-current-buffer)
(define-key dired-mode-map ")" 'dired-file-preview-mode)
(define-key dired-mode-map "_" 'dired-create-empty-file)
(put 'dired-find-alternate-file 'disabled nil) ; Don't let dired-mode create a new buffer for the dir
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;;; Web-mode
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq-default web-mode-markup-indent-offset 2 ; Indent of HTML
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2) ; Indent of JavaScript in HTML
  )

;;; emmet-mode
(use-package emmet-mode
  :hook (web-mode . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style " /")
  (define-key web-mode-map (kbd "C-/") #'emmet-expand-line))

;;; Term-mode
(use-package term
  :bind (:map term-raw-map
              ("M-:" . nil)
              ("M-x" . nil)
              ("C-x" . nil)
              ("C-q" . nil)
              ("C-y" . term-paste)
              ("C-h" . nil))
  :hook (term-mode . (lambda () (setq-local truncate-lines t))))

;;; Buffer Menu mode
(define-key Buffer-menu-mode-map "q" #'kill-current-buffer)

;;; eww
(use-package eww
  :bind (:map eww-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("v" . sniem-scroll-up-command)
              ("V" . sniem-scroll-down-command)
              ("/" . swiper)))

;;; Artist-mode
(use-package artist
  :hook (artist-mode . (lambda () (display-line-numbers-mode -1)))
  :config (advice-add 'artist-mode-exit :after
                      (lambda ()
                        (unless display-line-numbers-mode
                          (display-line-numbers-mode t)))))

;;; Markdown
(with-eval-after-load 'sniem
  (sniem-object-catch-mode-defalist markdown-mode
    ("`" . "`")))

(provide 'init-modes)
