;;;; The main config file for my configuration.

(add-to-list 'load-path "~/.emacs.d/etc/tools")
(add-to-list 'load-path "~/.emacs.d/etc/settings")
(add-to-list 'load-path "~/.emacs.d/etc/languages")

;;; Main Settings
(require 'init-basic)
(require 'init-ui)
(require 'init-keymaps)
(require 'init-functions)
(require 'init-package)
(require 'init-hugo)

;;; Languages
(require 'spring-c)
(require 'spring-python)
(require 'spring-go)
(require 'spring-js)
(require 'spring-dart)

;;; Other settings
;;; Tools
(require 'task-reminder)
(require 'run-code)
(require 'init-macros)
;;; Private settings
(when (file-exists-p (locate-user-emacs-file "private.el"))
  (load-file (locate-user-emacs-file "private.el"))
  (require 'private))

;;; Todo gets
(add-hook 'after-init-hook #'spring/todo-undo-p)

(provide 'init-config)
