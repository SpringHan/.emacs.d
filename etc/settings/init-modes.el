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
(gpack web-mode
  :hook (web-mode-hook . (lambda ()
                           (when (string-match-p "\\(.*\\).vue$" (buffer-name))
                             (lsp-deferred))))
  :config (progn
            (setq auto-mode-alist
                  (append '(("\\.html\\'" . web-mode)
                            ("\\.vue\\'" . web-mode))
                          auto-mode-alist))
            (setq-default web-mode-markup-indent-offset 2 ; Indent of HTML
                          web-mode-css-indent-offset 2
                          web-mode-code-indent-offset 2) ; Indent of JavaScript in HTML
            ))

;;; emmet-mode
(gpack emmet-mode
  :hook (web-mode-hook . emmet-mode)
  :var (emmet-self-closing-tag-style . " /")
  :config (define-key web-mode-map (kbd "C-/") #'emmet-expand-line))

;; Abbrev-mode
;; (setq-default abbrev-mode t) ; Open abbrev-mode
;; (define-abbrev-table
;;   'global-abbrev-table
;;   '(("MyName" "SpringHan")
;;     ("MyEmail" "<springchohaku@qq.com>")
;;     ("Copyright" "Copyright (C) <+++> SpringHan")))

;;; Term-mode
(gpack term
  :key (term-raw-map .
                     (("M-:" . nil)
                      ("M-x" . nil)
                      ("C-x" . nil)
                      ("C-q" . nil)
                      ("C-y" . term-paste)
                      ("C-h" . nil)))
  :hook (term-mode-hook . (lambda () (setq-local truncate-lines t))))

;;; Buffer Menu mode
(define-key Buffer-menu-mode-map "q" #'kill-current-buffer)

;;; eww
(gpack eww
  :key (eww-mode-map . (("n" . next-line)
                        ("p" . previous-line)
                        ("v" . sniem-scroll-up-command)
                        ("V" . sniem-scroll-down-command)
                        ("/" . swiper))))

;;; Artist-mode
(gpack artist
  :hook (artist-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :config (advice-add 'artist-mode-exit :after
                      (lambda ()
                        (unless display-line-numbers-mode
                          (display-line-numbers-mode t)))))

;;; Markdown
(sniem-object-catch-mode-defalist markdown-mode
  ("`" . "`"))

(provide 'init-modes)
