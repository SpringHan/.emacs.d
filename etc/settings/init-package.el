;;;; This file is used for packages configuration and more
;;; Set the plugin keybinding
(define-prefix-command 'plugin-key)
(define-prefix-command 'plugin-key-2)
(global-set-key (kbd "C-'") 'plugin-key)
(global-set-key (kbd "C--") 'plugin-key-2)

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(require 'package-require)							;Package Manager

;;; Packages
(require 'init-complete)
(require 'init-edits)
(require 'init-lsp)
(require 'init-modes)
(require 'init-tools)
(require 'init-awesome-tray)
(require 'init-git)
(require 'init-org)
(require 'init-evil)

(package-initialize)

;;;; Themes
(load-the-theme)

(provide 'init-package)
