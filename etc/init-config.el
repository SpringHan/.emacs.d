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

;;; Languages
(require 'spring-c)
(require 'spring-python)
(require 'spring-go)

;;; Other settings
;;; Tools
(require 'task-reminder)
(require 'run-code)
(require 'init-macros)

(provide 'init-config)
