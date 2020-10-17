;;;;; SpringHan's Emacs Configuration
;;; Mirror Config
(require 'package)
(require 'cl-lib) ; Common Lisp

;; GC
(setq gc-cons-threshold (* 50 1024 1024))

;;; Variables
(defvar spring/time-block nil
	"If the the time-block changed, it is t.
Otherwise it's nil.")
(defvar spring/unwanted-buffer
	'("*dashboard*" "notes.org" "tasks.org" "user-init.el" "*Help*" "*Backtrace*")
	"The buffers that I don't need.")

(load-file "~/.emacs.d/etc/init-config.el")
