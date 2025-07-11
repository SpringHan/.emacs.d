;;;; The main config file for my configuration.

(add-to-list 'load-path "~/.emacs.d/etc/tools")
(add-to-list 'load-path "~/.emacs.d/etc/settings")
(add-to-list 'load-path "~/.emacs.d/etc/languages")

;;; Init the empty lsp-mode.
(require 'lsp-mode)

;;; Main Settings
(require 'git-download)
(require 'init-basic)
(require 'init-ui)
(require 'init-keymaps)
(require 'init-functions)
(require 'init-package)

;;; Languages
(require 'spring-c)
(require 'spring-python)
(require 'spring-go)
(require 'spring-js)
(require 'spring-dart)
(require 'spring-clojure)
(require 'spring-rust)
(require 'spring-haskell)
(require 'spring-ts)

;;; Other settings
;;; Tools
(with-eval-after-load 'sniem
  (require 'emulting)
  (spring/extra-add-to-list "~/.emacs.d/etc/tools/emulting.el"))
;; (spring/native-compile-or-load "~/.emacs.d/etc/tools/emulting.el" t t)
(require 'run-code)
(require 'project-file)
(require 'init-macros)
(setq sniem-macro-file (locate-library "init-macros"))

(let ((private-file "~/.emacs.d/etc/init-private.el"))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;; Desktop save & restore.
(require 'desktop)
(add-hook 'kill-emacs-hook (lambda () (desktop-save "~/.emacs.d/var/")))
(add-hook 'emacs-startup-hook (lambda () (desktop-read "~/.emacs.d/var/")))
(add-hook 'desktop-after-read-hook #'load-the-theme)
(add-hook 'desktop-after-read-hook (lambda ()
                                     (setq kill-ring nil
                                           regexp-search-ring nil)))

;;; Natively Compile
(add-hook 'after-init-hook (lambda ()
                             (dolist (file spring/extra-native-compile-items)
                               (spring/native-compile-or-load file))
                             (setq spring/extra-items-compiled t)))

(provide 'init-config)
